{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Conduit
import Network.HTTP.Types (methodPost, methodDelete, status200, status204)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString.Lazy.Char8 as BL8

import Test.Tasty
import Test.Tasty.HUnit

-- =============================================================================
-- Test Configuration
-- =============================================================================

testVideoId :: T.Text
testVideoId = "dQw4w9WgXcQ"

-- =============================================================================
-- Types (copied from Main for standalone testing)
-- =============================================================================

data ClientSecrets = ClientSecrets
  { installed :: ClientConfig
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ClientConfig = ClientConfig
  { client_id :: T.Text
  , client_secret :: T.Text
  , auth_uri :: T.Text
  , token_uri :: T.Text
  , redirect_uris :: [T.Text]
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TokenResponse = TokenResponse
  { access_token :: T.Text
  , token_type :: T.Text
  , expires_in :: Maybe Int
  , refresh_token :: Maybe T.Text
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data StoredToken = StoredToken
  { stored_token :: TokenResponse
  , obtained_at :: Integer
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data CreatePlaylistRequest = CreatePlaylistRequest
  { cpr_snippet :: CreatePlaylistSnippet
  , cpr_status :: CreatePlaylistStatus
  } deriving stock (Show, Generic)

instance ToJSON CreatePlaylistRequest where
  toJSON r = object
    [ "snippet" .= cpr_snippet r
    , "status" .= cpr_status r
    ]

data CreatePlaylistSnippet = CreatePlaylistSnippet
  { cps_title :: T.Text
  , cps_description :: T.Text
  } deriving stock (Show, Generic)

instance ToJSON CreatePlaylistSnippet where
  toJSON s = object
    [ "title" .= cps_title s
    , "description" .= cps_description s
    ]

data CreatePlaylistStatus = CreatePlaylistStatus
  { cpst_privacyStatus :: T.Text
  } deriving stock (Show, Generic)

instance ToJSON CreatePlaylistStatus where
  toJSON s = object ["privacyStatus" .= cpst_privacyStatus s]

data CreatePlaylistResponse = CreatePlaylistResponse
  { cpr_id :: T.Text
  } deriving stock (Show, Generic)

instance FromJSON CreatePlaylistResponse where
  parseJSON = withObject "CreatePlaylistResponse" $ \v ->
    CreatePlaylistResponse <$> v .: "id"

data AddVideoRequest = AddVideoRequest
  { avr_snippet :: AddVideoSnippet
  } deriving stock (Show, Generic)

instance ToJSON AddVideoRequest where
  toJSON r = object ["snippet" .= avr_snippet r]

data AddVideoSnippet = AddVideoSnippet
  { avs_playlistId :: T.Text
  , avs_resourceId :: AddVideoResourceId
  } deriving stock (Show, Generic)

instance ToJSON AddVideoSnippet where
  toJSON s = object
    [ "playlistId" .= avs_playlistId s
    , "resourceId" .= avs_resourceId s
    ]

data AddVideoResourceId = AddVideoResourceId
  { avr_kind :: T.Text
  , avr_videoId :: T.Text
  } deriving stock (Show, Generic)

instance ToJSON AddVideoResourceId where
  toJSON r = object
    [ "kind" .= avr_kind r
    , "videoId" .= avr_videoId r
    ]

data PlaylistItemsResponse = PlaylistItemsResponse
  { pir_items :: [PlaylistItem]
  , pir_nextPageToken :: Maybe T.Text
  } deriving stock (Show, Generic)

instance FromJSON PlaylistItemsResponse where
  parseJSON = withObject "PlaylistItemsResponse" $ \v -> PlaylistItemsResponse
    <$> v .: "items"
    <*> v .:? "nextPageToken"

data PlaylistItem = PlaylistItem
  { plitem_id :: T.Text
  } deriving stock (Show, Generic)

instance FromJSON PlaylistItem where
  parseJSON = withObject "PlaylistItem" $ \v -> PlaylistItem
    <$> v .: "id"

-- =============================================================================
-- OAuth Functions
-- =============================================================================

tokenFileName :: FilePath
tokenFileName = "ytplaylist_token.json"

tokenFilePath :: IO FilePath
tokenFilePath = return tokenFileName

loadClientSecrets :: IO ClientSecrets
loadClientSecrets = do
  content <- BL.readFile "client_secrets.json"
  case eitherDecode content of
    Left err -> error $ "Failed to parse client_secrets.json: " ++ err
    Right cs -> return cs

getOrRefreshToken :: ClientConfig -> IO TokenResponse
getOrRefreshToken config = do
  tokenPath <- tokenFilePath
  exists <- doesFileExist tokenPath
  if not exists
    then error "Not authenticated"
    else do
      content <- BL.readFile tokenPath
      stored <- case eitherDecode content of
        Left err -> error $ "Failed to parse token: " ++ err
        Right t -> return (t :: StoredToken)
      now <- round <$> getPOSIXTime
      case expires_in (stored_token stored) of
        Just expSecs | (obtained_at stored + fromIntegral expSecs - 300) > now ->
          return $ stored_token stored
        _ -> case refresh_token (stored_token stored) of
          Just rt -> refreshAccessToken config rt
          Nothing -> error "Token expired"

refreshAccessToken :: ClientConfig -> T.Text -> IO TokenResponse
refreshAccessToken config rt = do
  let reqBody = "refresh_token=" ++ T.unpack rt ++
                "&client_id=" ++ T.unpack (client_id config) ++
                "&client_secret=" ++ T.unpack (client_secret config) ++
                "&grant_type=refresh_token"
  request <- parseRequest $ T.unpack $ token_uri config
  let request' = request
        { method = methodPost
        , requestBody = RequestBodyBS $ BL.toStrict $ BL8.pack reqBody
        , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
        }
  manager <- newManager tlsManagerSettings
  response <- httpLbs request' manager
  if responseStatus response == status200
    then case eitherDecode $ responseBody response of
      Left err -> error $ "Parse error: " ++ err
      Right token -> return token { refresh_token = case refresh_token token of
                                                      Nothing -> Just rt
                                                      Just t -> Just t }
    else error "Token refresh failed"

baseUrl :: String
baseUrl = "https://www.googleapis.com/youtube/v3"

-- =============================================================================
-- API Functions
-- =============================================================================

createPlaylist :: TokenResponse -> T.Text -> T.Text -> T.Text -> IO (Maybe T.Text)
createPlaylist token title desc privacy = do
  let url = baseUrl ++ "/playlists?part=snippet,status"
  let reqBody = CreatePlaylistRequest
        { cpr_snippet = CreatePlaylistSnippet title desc
        , cpr_status = CreatePlaylistStatus privacy
        }
  request <- parseRequest url
  let request' = request
        { method = methodPost
        , requestBody = RequestBodyLBS $ encode reqBody
        , requestHeaders =
            [ ("Authorization", "Bearer " <> BL.toStrict (BL8.pack $ T.unpack $ access_token token))
            , ("Content-Type", "application/json")
            ]
        }
  manager <- newManager tlsManagerSettings
  response <- httpLbs request' manager
  if responseStatus response == status200
    then case eitherDecode $ responseBody response :: Either String CreatePlaylistResponse of
      Left _ -> return Nothing
      Right resp -> return $ Just (cpr_id resp)
    else do
      putStrLn $ "Create error: " ++ show (responseStatus response)
      putStrLn $ BL8.unpack (responseBody response)
      return Nothing

addVideo :: TokenResponse -> T.Text -> T.Text -> IO Bool
addVideo token playlistId videoId = do
  let url = baseUrl ++ "/playlistItems?part=snippet"
  let reqBody = AddVideoRequest
        { avr_snippet = AddVideoSnippet
            { avs_playlistId = playlistId
            , avs_resourceId = AddVideoResourceId "youtube#video" videoId
            }
        }
  request <- parseRequest url
  let request' = request
        { method = methodPost
        , requestBody = RequestBodyLBS $ encode reqBody
        , requestHeaders =
            [ ("Authorization", "Bearer " <> BL.toStrict (BL8.pack $ T.unpack $ access_token token))
            , ("Content-Type", "application/json")
            ]
        }
  manager <- newManager tlsManagerSettings
  response <- httpLbs request' manager
  return $ responseStatus response == status200

fetchPlaylistItems :: TokenResponse -> T.Text -> IO [PlaylistItem]
fetchPlaylistItems token playlistId = fetchPages Nothing
  where
    fetchPages :: Maybe T.Text -> IO [PlaylistItem]
    fetchPages pageToken = do
      let base = baseUrl ++ "/playlistItems?part=id&playlistId=" ++ T.unpack playlistId ++ "&maxResults=50"
      let url = case pageToken of
                  Nothing -> base
                  Just tok -> base ++ "&pageToken=" ++ T.unpack tok
      request <- parseRequest url
      let request' = request
            { requestHeaders = [("Authorization", "Bearer " <> BL.toStrict (BL8.pack $ T.unpack $ access_token token))]
            }
      manager <- newManager tlsManagerSettings
      response <- httpLbs request' manager
      if responseStatus response == status200
        then case eitherDecode (responseBody response) :: Either String PlaylistItemsResponse of
          Left _ -> return []
          Right resp -> do
            rest <- case pir_nextPageToken resp of
              Nothing -> return []
              Just nextTok -> fetchPages (Just nextTok)
            return $ pir_items resp ++ rest
        else return []

removeVideo :: TokenResponse -> T.Text -> IO Bool
removeVideo token itemId = do
  let url = baseUrl ++ "/playlistItems?id=" ++ T.unpack itemId
  request <- parseRequest url
  let request' = request
        { method = methodDelete
        , requestHeaders = [("Authorization", "Bearer " <> BL.toStrict (BL8.pack $ T.unpack $ access_token token))]
        }
  manager <- newManager tlsManagerSettings
  response <- httpLbs request' manager
  return $ responseStatus response == status204

deletePlaylist :: TokenResponse -> T.Text -> IO Bool
deletePlaylist token playlistId = do
  let url = baseUrl ++ "/playlists?id=" ++ T.unpack playlistId
  request <- parseRequest url
  let request' = request
        { method = methodDelete
        , requestHeaders = [("Authorization", "Bearer " <> BL.toStrict (BL8.pack $ T.unpack $ access_token token))]
        }
  manager <- newManager tlsManagerSettings
  response <- httpLbs request' manager
  return $ responseStatus response == status204

-- =============================================================================
-- Tests
-- =============================================================================

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Placeholder test" $ True @?= True
  ]

integrationTests :: TestTree
integrationTests = testGroup "Integration Tests (requires auth)"
  [ testCaseSteps "Full playlist lifecycle" $ \step -> do
      hasSecrets <- doesFileExist "client_secrets.json"
      hasToken <- doesFileExist =<< tokenFilePath

      if not hasSecrets
        then do
          putStrLn "\nSkipping: client_secrets.json not found"
          return ()
        else if not hasToken
          then do
            putStrLn "\nSkipping: Not authenticated"
            return ()
          else runLifecycleTest step
  ]

runLifecycleTest :: (String -> IO ()) -> IO ()
runLifecycleTest step = do
  step "Loading credentials..."
  secrets <- loadClientSecrets
  token <- getOrRefreshToken (installed secrets)

  step "Creating test playlist..."
  mbPlaylistId <- createPlaylist token "Test Playlist" "Test description" "private"
  case mbPlaylistId of
    Nothing -> do
      putStrLn "FAILED: Could not create playlist"
      exitFailure
    Just playlistId -> do
      putStrLn $ "Created: " ++ T.unpack playlistId

      step "Adding video..."
      added <- addVideo token playlistId testVideoId
      if not added
        then do
          putStrLn "FAILED: Could not add video"
          _ <- deletePlaylist token playlistId
          exitFailure
        else putStrLn "Added successfully"

      step "Listing videos..."
      items <- fetchPlaylistItems token playlistId
      length items > 0 @? "Should have at least one item"
      putStrLn $ "Found " ++ show (length items) ++ " item(s)"

      step "Removing video..."
      let itemId = plitem_id $ head items
      removed <- removeVideo token itemId
      if not removed
        then do
          putStrLn "FAILED: Could not remove"
          _ <- deletePlaylist token playlistId
          exitFailure
        else putStrLn "Removed successfully"

      step "Deleting playlist..."
      deleted <- deletePlaylist token playlistId
      if deleted
        then putStrLn "Cleanup successful"
        else putStrLn "Note: Could not delete playlist"

      step "Test completed!"

main :: IO ()
main = do
  putStrLn "\n========================================"
  putStrLn "YouTube Playlist Manager - Test Suite"
  putStrLn "========================================\n"

  defaultMain $ testGroup "All Tests"
    [ unitTests
    , integrationTests
    ]
