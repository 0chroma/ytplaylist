{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module OAuth
  ( -- Configuration
    secretsPath
  , tokenFileName
  , tokenFilePath
    -- OAuth types (re-exported from hoauth2)
  , OAuth2(..)
  , OAuth2Token(..)
  , AccessToken(..)
  , RefreshToken(..)
    -- OAuth functions
  , loadClientSecrets
  , getOrRefreshToken
  , refreshAccessToken
  , saveToken
  , authenticateInteractive
  ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Network.HTTP.Client.TLS (getGlobalManager)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Web.Browser (openBrowser)
import Control.Monad.Trans.Except (runExceptT)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Network.OAuth.OAuth2 (OAuth2(..), OAuth2Token(..), AccessToken(..), RefreshToken(..), ExchangeToken(..), QueryParams)
import Network.OAuth.OAuth2.AuthorizationRequest (authorizationUrlWithParams)
import qualified Network.OAuth.OAuth2.TokenRequest as TokenRequest
import URI.ByteString.QQ (uri)

-- =============================================================================
-- Configuration
-- =============================================================================

secretsPath :: FilePath
secretsPath = "client_secrets.json"

tokenFileName :: FilePath
tokenFileName = "ytplaylist_token.json"

tokenFilePath :: IO FilePath
tokenFilePath = return tokenFileName

-- Google OAuth2 endpoints
googleOAuth2 :: T.Text -> T.Text -> T.Text -> OAuth2
googleOAuth2 clientId clientSecret _redirectUri = OAuth2
  { oauth2ClientId = clientId
  , oauth2ClientSecret = clientSecret
  , oauth2AuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/v2/auth|]
  , oauth2TokenEndpoint = [uri|https://oauth2.googleapis.com/token|]
  , oauth2RedirectUri = [uri|urn:ietf:wg:oauth:2.0:oob|]  -- Out-of-band for CLI apps
  }

-- =============================================================================
-- Stored Token (for persistence)
-- =============================================================================

data StoredToken = StoredToken
  { storedToken :: OAuth2Token
  , obtainedAt :: Integer
  } deriving (Show, Generic)

instance ToJSON StoredToken
instance FromJSON StoredToken

-- =============================================================================
-- Client Secrets
-- =============================================================================

data ClientSecrets = ClientSecrets
  { installed :: ClientConfig
  } deriving (Show, Generic)

instance FromJSON ClientSecrets

data ClientConfig = ClientConfig
  { client_id :: T.Text
  , client_secret :: T.Text
  , redirect_uris :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON ClientConfig

-- =============================================================================
-- OAuth Functions
-- =============================================================================

loadClientSecrets :: IO OAuth2
loadClientSecrets = do
  content <- BL.readFile secretsPath
  case eitherDecode content of
    Left err -> error $ "Failed to parse client_secrets.json: " ++ err
    Right (cs :: ClientSecrets) -> do
      let config = installed cs
      return $ googleOAuth2 (client_id config) (client_secret config) (head $ redirect_uris config)

getOrRefreshToken :: OAuth2 -> IO OAuth2Token
getOrRefreshToken oauth2Config = do
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
      case expiresIn (storedToken stored) of
        Just expSecs | (obtainedAt stored + fromIntegral expSecs - 300) > now ->
          return $ storedToken stored
        _ -> case refreshToken (storedToken stored) of
          Just rt -> refreshAccessToken' oauth2Config rt
          Nothing -> error "Token expired. Run: ytplaylist auth"

refreshAccessToken' :: OAuth2 -> RefreshToken -> IO OAuth2Token
refreshAccessToken' oauth2Config rt = do
  manager <- getGlobalManager
  result <- runExceptT $ TokenRequest.refreshAccessToken manager oauth2Config rt
  case result of
    Left err -> error $ "Token refresh failed: " ++ show err
    Right token -> do
      let token' = case refreshToken token of
                     Nothing -> token { refreshToken = Just rt }
                     Just _ -> token
      saveToken token'
      return token'

refreshAccessToken :: OAuth2 -> RefreshToken -> IO OAuth2Token
refreshAccessToken = refreshAccessToken'

saveToken :: OAuth2Token -> IO ()
saveToken tok = do
  now <- round <$> getPOSIXTime
  let stored = StoredToken tok now
  tokenPath <- tokenFilePath
  BL.writeFile tokenPath $ encodePretty stored

authenticateInteractive :: OAuth2 -> IO OAuth2Token
authenticateInteractive oauth2Config = do
  let queryParams :: QueryParams
      queryParams =
        [ ("response_type", "code")
        , ("scope", "https://www.googleapis.com/auth/youtube")
        , ("prompt", "consent")
        , ("access_type", "offline")
        ]
      authUrl = authorizationUrlWithParams queryParams oauth2Config

  putStrLn "\nOpening browser for authentication..."
  putStrLn $ "If browser doesn't open, visit:\n" ++ show authUrl ++ "\n"
  _ <- openBrowser (show authUrl)
  putStr "Enter the authorization code: "
  hFlush stdout
  code <- getLine

  manager <- getGlobalManager
  result <- runExceptT $ TokenRequest.fetchAccessToken manager oauth2Config (ExchangeToken $ T.pack code)
  case result of
    Left err -> error $ "Token exchange failed: " ++ show err
    Right token -> do
      saveToken token
      return token
