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
import Data.Maybe (fromJust)
import Text.URI (mkURI)

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

parseUrl :: String -> (Url 'Https, Option 'Https)
parseUrl urlStr = fromJust $ do
  uri <- mkURI (T.pack urlStr)
  useHttpsURI uri

authHeader :: AccessToken -> Option 'Https
authHeader (AccessToken token) = header "Authorization" ("Bearer " <> encodeUtf8 token)

getJSON :: (FromJSON a) => AccessToken -> String -> IO (Either String a)
getJSON token urlStr = runReq defaultHttpConfig $ do
  let (url, opts) = parseUrl urlStr
  response <- req GET url NoReqBody lbsResponse (authHeader token <> opts)
  if responseStatusCode response == 200
    then return $ eitherDecode (responseBody response)
    else return $ Left $ "HTTP " ++ show (responseStatusCode response)

postJSON :: (ToJSON a, FromJSON b) => AccessToken -> String -> a -> IO (Either String b)
postJSON token urlStr body = runReq defaultHttpConfig $ do
  let (url, opts) = parseUrl urlStr
  response <- req POST url (ReqBodyJson body) lbsResponse (authHeader token <> opts)
  if responseStatusCode response == 200
    then return $ eitherDecode (responseBody response)
    else return $ Left $ "HTTP " ++ show (responseStatusCode response)

deleteRequest :: AccessToken -> String -> IO Bool
deleteRequest token urlStr = runReq defaultHttpConfig $ do
  let (url, opts) = parseUrl urlStr
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

batchRequest :: AccessToken -> [BS8.ByteString] -> IO [Bool]
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
  if responseStatus response == status200
    then return $ parseBatchResponse $ HC.responseBody response
    else return $ replicate (length subRequests) False
  where
    hashBoundary reqs = sum $ map (fromEnum . BS8.head . BS8.take 1) reqs

parseBatchResponse :: BL.ByteString -> [Bool]
parseBatchResponse body =
  let parts = BL.split (fromIntegral $ fromEnum '-') body
  in map checkPart parts
  where
    checkPart part =
      let partStr = BS8.unpack $ BL.toStrict part
      in "HTTP/1.1 200" `isInfixOf` partStr || "HTTP/1.1 204" `isInfixOf` partStr

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
