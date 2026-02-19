{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module HTTP
  ( -- Configuration
    baseUrl
  , batchUrl
    -- HTTP helpers
  , getJSON
  , postJSON
  , deleteRequest
    -- Batch operations
  , BatchResult(..)
  , batchRequest
  , buildAddSubRequest
  , buildDeleteSubRequest
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (RequestBody(..), parseRequest, responseStatus, requestHeaders, method, requestBody, httpLbs)
import qualified Network.HTTP.Client as HC (responseBody)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types (status200)
import Data.List (isInfixOf)
import qualified Network.URI.Encode as URI
import Network.HTTP.Req (Url, Option, Scheme(..), GET(..), POST(..), DELETE(..), lbsResponse, NoReqBody(..), ReqBodyJson(..), runReq, defaultHttpConfig, req, useHttpsURI, header, responseStatusCode, responseBody)
import Text.URI (mkURI)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)

import Network.OAuth.OAuth2 (AccessToken(..))
import Types

-- =============================================================================
-- Configuration
-- =============================================================================

baseUrl :: String
baseUrl = "https://www.googleapis.com/youtube/v3"

batchUrl :: String
batchUrl = "https://www.googleapis.com/batch/youtube/v3"

-- =============================================================================
-- HTTP Helpers
-- =============================================================================

newtype UrlParseError = UrlParseError String
  deriving (Show)

instance Exception UrlParseError

parseUrl :: String -> IO (Url 'Https, Option 'Https)
parseUrl urlStr = case mkURI (T.pack urlStr) >>= useHttpsURI of
  Nothing -> throwIO $ UrlParseError $ "Failed to parse URL: " ++ urlStr
  Just result -> return result

authHeader :: AccessToken -> Option 'Https
authHeader (AccessToken token) = header "Authorization" ("Bearer " <> encodeUtf8 token)

getJSON :: (FromJSON a) => AccessToken -> String -> IO (Either String a)
getJSON token urlStr = runReq defaultHttpConfig $ do
  (url, opts) <- liftIO $ parseUrl urlStr
  response <- req GET url NoReqBody lbsResponse (authHeader token <> opts)
  if responseStatusCode response == 200
    then return $ eitherDecode (responseBody response)
    else return $ Left $ "HTTP " ++ show (responseStatusCode response)

postJSON :: (ToJSON a, FromJSON b) => AccessToken -> String -> a -> IO (Either String b)
postJSON token urlStr body = runReq defaultHttpConfig $ do
  (url, opts) <- liftIO $ parseUrl urlStr
  response <- req POST url (ReqBodyJson body) lbsResponse (authHeader token <> opts)
  if responseStatusCode response == 200
    then return $ eitherDecode (responseBody response)
    else return $ Left $ "HTTP " ++ show (responseStatusCode response)

deleteRequest :: AccessToken -> String -> IO Bool
deleteRequest token urlStr = runReq defaultHttpConfig $ do
  (url, opts) <- liftIO $ parseUrl urlStr
  response <- req DELETE url NoReqBody lbsResponse (authHeader token <> opts)
  return $ responseStatusCode response == 204

-- =============================================================================
-- Batch HTTP Helpers (uses http-client directly for multipart)
-- =============================================================================

buildBatchBody :: String -> [BS8.ByteString] -> BS8.ByteString
buildBatchBody boundary parts = BS8.concat $
  [ BS8.pack $ "--" ++ boundary ++ "\r\n"
  , "Content-Type: application/http\r\n\r\n"
  ] ++
  concatMap (\part ->
    [ part
    , BS8.pack $ "\r\n--" ++ boundary ++ "\r\n"
    , "Content-Type: application/http\r\n\r\n"
    ]
  ) (init parts) ++
  [ last parts
  , BS8.pack $ "\r\n--" ++ boundary ++ "--"
  ]

batchRequest :: AccessToken -> [BS8.ByteString] -> IO [BatchResult]
batchRequest token subRequests = do
  let boundary = "batch_" ++ take 16 (filter (`elem` (['a'..'z'] ++ ['0'..'9'])) $ show $ hashBoundary subRequests)
      body = buildBatchBody boundary subRequests
      authBS = case token of AccessToken t -> "Bearer " <> encodeUtf8 t
  manager <- getGlobalManager
  request <- parseRequest batchUrl
  let request' = request
        { method = "POST"
        , requestBody = RequestBodyBS body
        , requestHeaders =
          [ ("Authorization", authBS)
          , ("Content-Type", BS8.pack $ "multipart/mixed; boundary=" ++ boundary)
          ]
        }
  response <- httpLbs request' manager
  let respBody = HC.responseBody response
  if responseStatus response == status200
    then parseBatchResponse boundary respBody
    else return $ replicate (length subRequests) (BatchResult False (Just "HTTP batch request failed"))
  where
    hashBoundary reqs = sum $ map (fromEnum . BS8.head . BS8.take 1) reqs

data BatchResult = BatchResult
  { batchSuccess :: Bool
  , batchError :: Maybe String
  } deriving (Show)

parseBatchResponse :: String -> BL.ByteString -> IO [BatchResult]
parseBatchResponse _ body
  | BL.null body = return [BatchResult False (Just "Empty batch response")]
  | otherwise = do
  case extractBoundary body of
    Nothing -> return [BatchResult False (Just "Could not extract boundary from response")]
    Just boundary -> do
      let boundaryBytes = BS8.pack $ "--" ++ boundary
          parts = splitOnLazy boundaryBytes body
          filteredParts = filter (not . BL.null) $ map (BL.drop 2) $ drop 1 parts
      if null filteredParts
        then return [BatchResult False (Just "No parts found in batch response")]
        else mapM checkPart filteredParts
  where
    extractBoundary :: BL.ByteString -> Maybe String
    extractBoundary bs = do
      let bodyStr = BS8.unpack $ BL.toStrict bs
          trimmedBody = dropWhile (`elem` ("\n\r" :: String)) bodyStr
          firstLine = takeWhile (`notElem` ("\n\r" :: String)) trimmedBody
      if take 2 firstLine == "--"
        then Just (drop 2 firstLine)
        else case dropWhile (not . isPrefixOf "--") (lines bodyStr) of
          (line:_) -> Just $ drop 2 line
          [] -> Nothing
      where
        isPrefixOf p s = take (length p) s == p

    checkPart :: BL.ByteString -> IO BatchResult
    checkPart part
      | BL.null part = return $ BatchResult False (Just "Empty response part")
      | otherwise = do
        let partStr = BS8.unpack $ BL.toStrict part
            isSuccess = "HTTP/1.1 200" `isInfixOf` partStr || "HTTP/1.1 204" `isInfixOf` partStr
            errorMsg = if isSuccess then Nothing else extractError partStr
        return $ BatchResult isSuccess errorMsg
    
    extractError :: String -> Maybe String
    extractError partStr =
      let httpStatus = extractHTTPStatus partStr
          jsonError = extractJSONError partStr
      in case (httpStatus, jsonError) of
           (Just status, Just msg) -> Just $ status ++ ": " ++ msg
           (Just status, Nothing) -> Just status
           (Nothing, Just msg) -> Just msg
           (Nothing, Nothing) -> Just "Unknown error"
    
    extractHTTPStatus :: String -> Maybe String
    extractHTTPStatus partStr =
      case filter ("HTTP/1.1" `isInfixOf`) $ lines partStr of
        (line:_) -> Just $ trim $ drop 9 line
        [] -> Nothing
      where trim = reverse . dropWhile (`elem` ("\r\n " :: String)) . reverse . dropWhile (`elem` ("\r\n " :: String))
    
    extractJSONError :: String -> Maybe String
    extractJSONError partStr =
      case filter ("\"message\":" `isInfixOf`) $ lines partStr of
        (line:_) -> Just $ cleanMessage $ extractStringValue line
        [] -> Nothing
    
    cleanMessage :: String -> String
    cleanMessage msg = 
      let withoutUnicode = takeWhile (\c -> c /= '\\' && c /= '<') msg
      in if null withoutUnicode then msg else withoutUnicode
    
    extractStringValue :: String -> String
    extractStringValue line =
      let afterColon = dropWhile (/= ':') line
          afterQuote = drop 1 $ dropWhile (/= '"') afterColon
          value = takeWhile (/= '"') afterQuote
      in value

-- | Split a lazy ByteString on a strict ByteString pattern
splitOnLazy :: BS8.ByteString -> BL.ByteString -> [BL.ByteString]
splitOnLazy pattern = go BL.empty
  where
    patternLen = BS8.length pattern
    go acc bs
      | BL.null bs = [acc]
      | otherwise =
          let candidate = BL.toStrict $ BL.take (fromIntegral patternLen) bs
          in if candidate == pattern
             then acc : go BL.empty (BL.drop (fromIntegral patternLen) bs)
             else go (acc `BL.append` BL.take 1 bs) (BL.drop 1 bs)

buildAddSubRequest :: T.Text -> T.Text -> BS8.ByteString
buildAddSubRequest playlistId videoId =
  let url = "/youtube/v3/playlistItems?part=snippet"
      body = encode $ AddVideoRequest
        { avr_snippet = AddVideoSnippet
            { avs_playlistId = playlistId
            , avs_resourceId = AddVideoResourceId "youtube#video" videoId
            }
        }
  in BS8.concat
    [ "POST "
    , BS8.pack url
    , " HTTP/1.1\r\n"
    , "Content-Type: application/json\r\n\r\n"
    , BL.toStrict body
    ]

buildDeleteSubRequest :: T.Text -> BS8.ByteString
buildDeleteSubRequest itemId =
  BS8.pack $ "DELETE /youtube/v3/playlistItems?id=" ++ URI.encode (T.unpack itemId) ++ " HTTP/1.1\r\n\r\n"
