{-# LANGUAGE OverloadedStrings #-}

module OAuth
  ( -- Configuration
    secretsPath
  , tokenFileName
  , tokenFilePath
    -- OAuth functions
  , loadClientSecrets
  , getOrRefreshToken
  , refreshAccessToken
  , saveToken
  , authenticateInteractive
  ) where

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (methodPost, status200, hContentType)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import System.Process (proc, createProcess, waitForProcess, std_out, std_err, StdStream(..))
import Control.Exception (try, SomeException)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.URI.Encode as URI

import Types

-- =============================================================================
-- Configuration
-- =============================================================================

secretsPath :: FilePath
secretsPath = "client_secrets.json"

tokenFileName :: FilePath
tokenFileName = "ytplaylist_token.json"

tokenFilePath :: IO FilePath
tokenFilePath = return tokenFileName

-- =============================================================================
-- OAuth Functions
-- =============================================================================

loadClientSecrets :: IO ClientSecrets
loadClientSecrets = do
  content <- BL.readFile secretsPath
  case eitherDecode content of
    Left err -> error $ "Failed to parse client_secrets.json: " ++ err
    Right cs -> return cs

getOrRefreshToken :: ClientConfig -> IO TokenResponse
getOrRefreshToken config = do
  tokenPath <- tokenFilePath
  exists <- doesFileExist tokenPath
  if not exists
    then error $ "Authentication token not found: " ++ tokenFileName ++ "\nRun: ytplaylist auth"
    else do
      content <- BL.readFile tokenPath
      stored <- case eitherDecode content of
        Left err -> error $ "Failed to parse token file: " ++ err
        Right t -> return (t :: StoredToken)
      now <- round <$> getPOSIXTime
      case expires_in (stored_token stored) of
        Just expSecs | (obtained_at stored + fromIntegral expSecs - 300) > now ->
          return $ stored_token stored
        _ -> case refresh_token (stored_token stored) of
          Just rt -> refreshAccessToken config rt
          Nothing -> error "Token expired. Run: ytplaylist auth"

refreshAccessToken :: ClientConfig -> T.Text -> IO TokenResponse
refreshAccessToken config rt = do
  let reqBody = "refresh_token=" ++ URI.encode (T.unpack rt) ++
                "&client_id=" ++ URI.encode (T.unpack $ client_id config) ++
                "&client_secret=" ++ URI.encode (T.unpack $ client_secret config) ++
                "&grant_type=refresh_token"
  request <- parseRequest $ T.unpack $ token_uri config
  let request' = request
        { method = methodPost
        , requestBody = RequestBodyBS $ BS8.pack reqBody
        , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
        }
  manager <- newManager tlsManagerSettings
  response <- httpLbs request' manager
  if responseStatus response == status200
    then case eitherDecode $ responseBody response of
      Left err -> error $ "Failed to parse refresh response: " ++ err
      Right token -> do
        let token' = token { refresh_token = case refresh_token token of
                                               Nothing -> Just rt
                                               Just t -> Just t }
        saveToken token'
        return token'
    else error $ "Token refresh failed: " ++ show (responseBody response)

saveToken :: TokenResponse -> IO ()
saveToken tok = do
  now <- round <$> getPOSIXTime
  let stored = StoredToken tok now
  tokenPath <- tokenFilePath
  BL.writeFile tokenPath $ encodePretty stored

authenticateInteractive :: ClientConfig -> IO TokenResponse
authenticateInteractive config = do
  let authUrl = T.unpack (auth_uri config) ++
        "?client_id=" ++ URI.encode (T.unpack $ client_id config) ++
        "&redirect_uri=" ++ URI.encode (T.unpack $ head $ redirect_uris config) ++
        "&response_type=code" ++
        "&scope=" ++ URI.encode "https://www.googleapis.com/auth/youtube" ++
        "&access_type=offline" ++
        "&prompt=consent"
  putStrLn "\nOpening browser for authentication..."
  putStrLn $ "If browser doesn't open, visit:\n" ++ authUrl ++ "\n"
  _ <- tryOpenBrowser authUrl
  putStr "Enter the authorization code: "
  hFlush stdout
  code <- getLine
  let reqBody = "code=" ++ URI.encode code ++
                "&client_id=" ++ URI.encode (T.unpack $ client_id config) ++
                "&client_secret=" ++ URI.encode (T.unpack $ client_secret config) ++
                "&redirect_uri=" ++ URI.encode (T.unpack $ head $ redirect_uris config) ++
                "&grant_type=authorization_code"
  request <- parseRequest $ T.unpack $ token_uri config
  let request' = request
        { method = methodPost
        , requestBody = RequestBodyBS $ BS8.pack reqBody
        , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
        }
  manager <- newManager tlsManagerSettings
  response <- httpLbs request' manager
  if responseStatus response == status200
    then case eitherDecode $ responseBody response of
      Left err -> error $ "Failed to parse token response: " ++ err
      Right token -> do
        saveToken token
        return token
    else error $ "Token exchange failed: " ++ show (responseBody response)

-- =============================================================================
-- Helper Functions
-- =============================================================================

tryOpenBrowser :: String -> IO ()
tryOpenBrowser url = go
  [ ("xdg-open", [url])
  , ("open", [url])
  , ("firefox", [url])
  , ("google-chrome", [url])
  ]
  where
    go [] = return ()
    go ((cmd, args):rest) = do
      success <- tryCommand cmd args
      if success then return () else go rest
    tryCommand cmd args = do
      result <- try (callProcessSilent cmd args) :: IO (Either SomeException ())
      return $ case result of
        Left _ -> False
        Right _ -> True

callProcessSilent :: String -> [String] -> IO ()
callProcessSilent cmd args = do
  (_, _, _, ph) <- createProcess (proc cmd args)
    { std_out = NoStream, std_err = NoStream }
  _ <- waitForProcess ph
  return ()
