{-# LANGUAGE OverloadedStrings #-}

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
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (methodPost, methodDelete, status200, status204, hAuthorization, hContentType)
import Data.List (isInfixOf)
import qualified Network.URI.Encode as URI

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

authHeader :: AccessToken -> BS8.ByteString
authHeader (AccessToken token) = "Bearer " <> encodeUtf8 token

getJSON :: (FromJSON a) => AccessToken -> String -> IO (Either String a)
getJSON token urlStr = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest urlStr
  let request' = request { requestHeaders = [(hAuthorization, authHeader token)] }
  response <- httpLbs request' manager
  if responseStatus response == status200
    then return $ eitherDecode (responseBody response)
    else return $ Left $ "HTTP " ++ show (responseStatus response)

postJSON :: (ToJSON a, FromJSON b) => AccessToken -> String -> a -> IO (Either String b)
postJSON token urlStr body = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest urlStr
  let request' = request
        { method = methodPost
        , requestBody = RequestBodyLBS $ encode body
        , requestHeaders = [(hAuthorization, authHeader token), (hContentType, "application/json")]
        }
  response <- httpLbs request' manager
  if responseStatus response == status200
    then return $ eitherDecode (responseBody response)
    else return $ Left $ "HTTP " ++ show (responseStatus response)

deleteRequest :: AccessToken -> String -> IO Bool
deleteRequest token urlStr = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest urlStr
  let request' = request { method = methodDelete, requestHeaders = [(hAuthorization, authHeader token)] }
  response <- httpLbs request' manager
  return $ responseStatus response == status204

-- =============================================================================
-- Batch HTTP Helpers
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
  manager <- newManager tlsManagerSettings
  request <- parseRequest batchUrl
  let request' = request
        { method = methodPost
        , requestBody = RequestBodyBS body
        , requestHeaders =
          [ (hAuthorization, authHeader token)
          , (hContentType, BS8.pack $ "multipart/mixed; boundary=" ++ boundary)
          ]
        }
  response <- httpLbs request' manager
  if responseStatus response == status200
    then return $ parseBatchResponse $ responseBody response
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
