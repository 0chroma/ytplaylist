{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (methodPost, methodDelete, status200, status204, hAuthorization, hContentType)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)

import Control.Exception (try, SomeException)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (find)
import System.Process (proc, createProcess, waitForProcess, std_out, std_err, StdStream(..))


-- =============================================================================
-- Types
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

-- Playlist types
newtype PlaylistsResponse = PlaylistsResponse
  { pr_items :: [PlaylistInfo]
  } deriving stock (Show, Generic)

instance FromJSON PlaylistsResponse where
  parseJSON = withObject "PlaylistsResponse" $ \v ->
    PlaylistsResponse <$> v .: "items"

data PlaylistInfo = PlaylistInfo
  { pli_id :: T.Text
  , pli_snippet :: PlaylistSnippetInfo
  , pli_contentDetails :: PlaylistContentDetails
  } deriving stock (Show, Generic)

instance FromJSON PlaylistInfo where
  parseJSON = withObject "PlaylistInfo" $ \v -> PlaylistInfo
    <$> v .: "id"
    <*> v .: "snippet"
    <*> v .: "contentDetails"

data PlaylistSnippetInfo = PlaylistSnippetInfo
  { plsi_title :: T.Text
  } deriving stock (Show, Generic)

instance FromJSON PlaylistSnippetInfo where
  parseJSON = withObject "PlaylistSnippetInfo" $ \v ->
    PlaylistSnippetInfo <$> v .: "title"

data PlaylistContentDetails = PlaylistContentDetails
  { plcd_itemCount :: Int
  } deriving stock (Show, Generic)

instance FromJSON PlaylistContentDetails where
  parseJSON = withObject "PlaylistContentDetails" $ \v ->
    PlaylistContentDetails <$> v .: "itemCount"

-- Playlist item types
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
  , plitem_snippet :: PlaylistItemSnippet
  } deriving stock (Show, Generic)

instance FromJSON PlaylistItem where
  parseJSON = withObject "PlaylistItem" $ \v -> PlaylistItem
    <$> v .: "id"
    <*> v .: "snippet"

data PlaylistItemSnippet = PlaylistItemSnippet
  { plitem_title :: T.Text
  , plitem_resourceId :: ResourceId
  } deriving stock (Show, Generic)

instance FromJSON PlaylistItemSnippet where
  parseJSON = withObject "PlaylistItemSnippet" $ \v -> PlaylistItemSnippet
    <$> v .: "title"
    <*> v .: "resourceId"

data ResourceId = ResourceId
  { res_videoId :: T.Text
  } deriving stock (Show, Generic)

instance FromJSON ResourceId where
  parseJSON = withObject "ResourceId" $ \v ->
    ResourceId <$> v .: "videoId"

-- Create playlist types
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

-- Add video types
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

-- =============================================================================
-- Configuration
-- =============================================================================

secretsPath :: FilePath
secretsPath = "client_secrets.json"

tokenFileName :: FilePath
tokenFileName = "ytplaylist_token.json"

tokenFilePath :: IO FilePath
tokenFilePath = return tokenFileName

baseUrl :: String
baseUrl = "https://www.googleapis.com/youtube/v3"

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
  let reqBody = "refresh_token=" ++ escapeURIComponent (T.unpack rt) ++
                "&client_id=" ++ escapeURIComponent (T.unpack $ client_id config) ++
                "&client_secret=" ++ escapeURIComponent (T.unpack $ client_secret config) ++
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
        "?client_id=" ++ escapeURIComponent (T.unpack $ client_id config) ++
        "&redirect_uri=" ++ escapeURIComponent (T.unpack $ head $ redirect_uris config) ++
        "&response_type=code" ++
        "&scope=" ++ escapeURIComponent "https://www.googleapis.com/auth/youtube" ++
        "&access_type=offline" ++
        "&prompt=consent"
  putStrLn "\nOpening browser for authentication..."
  putStrLn $ "If browser doesn't open, visit:\n" ++ authUrl ++ "\n"
  _ <- tryOpenBrowser authUrl
  putStr "Enter the authorization code: "
  hFlush stdout
  code <- getLine
  let reqBody = "code=" ++ escapeURIComponent code ++
                "&client_id=" ++ escapeURIComponent (T.unpack $ client_id config) ++
                "&client_secret=" ++ escapeURIComponent (T.unpack $ client_secret config) ++
                "&redirect_uri=" ++ escapeURIComponent (T.unpack $ head $ redirect_uris config) ++
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

escapeURIComponent :: String -> String
escapeURIComponent = concatMap encodeChar
  where
    encodeChar c
      | c >= 'A' && c <= 'Z' = [c]
      | c >= 'a' && c <= 'z' = [c]
      | c >= '0' && c <= '9' = [c]
      | c `elem` ("-_.~" :: String) = [c]
      | otherwise = '%' : toHex (fromEnum c)
    toHex n = let h = n `div` 16; l = n `mod` 16
              in [toHexDigit h, toHexDigit l]
    toHexDigit d = if d < 10 then toEnum (d + fromEnum '0')
                             else toEnum (d - 10 + fromEnum 'A')

-- =============================================================================
-- HTTP Helpers (using http-client)
-- =============================================================================

-- | Authorization header value
authHeader :: TokenResponse -> BS8.ByteString
authHeader token = "Bearer " <> encodeUtf8 (access_token token)

-- | Make an authenticated GET request and parse JSON response
getJSON :: (FromJSON a) => TokenResponse -> String -> IO (Either String a)
getJSON token urlStr = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest urlStr
  let request' = request { requestHeaders = [(hAuthorization, authHeader token)] }
  response <- httpLbs request' manager
  if responseStatus response == status200
    then return $ eitherDecode (responseBody response)
    else return $ Left $ "HTTP " ++ show (responseStatus response)

-- | Make an authenticated POST request with JSON body
postJSON :: (ToJSON a, FromJSON b) => TokenResponse -> String -> a -> IO (Either String b)
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

-- | Make an authenticated DELETE request
deleteRequest :: TokenResponse -> String -> IO Bool
deleteRequest token urlStr = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest urlStr
  let request' = request { method = methodDelete, requestHeaders = [(hAuthorization, authHeader token)] }
  response <- httpLbs request' manager
  return $ responseStatus response == status204

-- =============================================================================
-- API Functions
-- =============================================================================

-- | List all user's playlists
listPlaylists :: TokenResponse -> IO ()
listPlaylists token = do
  let url = baseUrl ++ "/playlists?part=snippet,contentDetails&mine=true&maxResults=50"
  result <- getJSON token url :: IO (Either String PlaylistsResponse)
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ err
    Right resp -> do
      putStrLn "\n=== Your Playlists ===\n"
      mapM_ printPlaylist (pr_items resp)
      putStrLn $ "\nTotal: " ++ show (length $ pr_items resp) ++ " playlists"
  where
    printPlaylist pl = do
      putStrLn $ T.unpack (plsi_title $ pli_snippet pl)
      putStrLn $ "  ID: " ++ T.unpack (pli_id pl)
      putStrLn $ "  Videos: " ++ show (plcd_itemCount $ pli_contentDetails pl)
      putStrLn ""

-- | Fetch all playlist items with pagination
fetchPlaylistItems :: TokenResponse -> T.Text -> IO [PlaylistItem]
fetchPlaylistItems token playlistId = fetchPages Nothing
  where
    fetchPages :: Maybe T.Text -> IO [PlaylistItem]
    fetchPages pageToken = do
      let base = baseUrl ++ "/playlistItems?part=snippet&playlistId=" ++ T.unpack playlistId ++ "&maxResults=50"
          url = case pageToken of
                  Nothing -> base
                  Just tok -> base ++ "&pageToken=" ++ escapeURIComponent (T.unpack tok)
      result <- getJSON token url :: IO (Either String PlaylistItemsResponse)
      case result of
        Left _ -> return []
        Right resp -> do
          rest <- case pir_nextPageToken resp of
            Nothing -> return []
            Just nextTok -> fetchPages (Just nextTok)
          return $ pir_items resp ++ rest

-- | List all videos in a playlist
listPlaylistVideos :: TokenResponse -> T.Text -> IO ()
listPlaylistVideos token playlistId = do
  items <- fetchPlaylistItems token playlistId
  putStrLn $ "\n=== Playlist Videos ==="
  putStrLn $ "Playlist ID: " ++ T.unpack playlistId
  putStrLn $ "Total: " ++ show (length items) ++ " videos\n"
  putStrLn $ padL 4 "#" ++ "  " ++ padR 20 "Item ID" ++ "  " ++ padR 15 "Video ID" ++ "  Title"
  putStrLn $ replicate 90 '-'
  mapM_ printItem (zip [1..] items)
  where
    printItem (idx, item) = do
      let num = padL 4 (show (idx :: Int))
      let itemId = padR 20 (T.unpack $ plitem_id item)
      let vidId = padR 15 (T.unpack $ res_videoId $ plitem_resourceId $ plitem_snippet item)
      let title = trunc 40 (T.unpack $ plitem_title $ plitem_snippet item)
      putStrLn $ num ++ "  " ++ itemId ++ "  " ++ vidId ++ "  " ++ title
    padL n s = replicate (max 0 (n - length s)) ' ' ++ s
    padR n s = s ++ replicate (max 0 (n - length s)) ' '
    trunc n s = if length s > n then take (n-3) s ++ "..." else s

-- | Remove a video from playlist (by playlist item ID)
removeVideo :: TokenResponse -> T.Text -> IO Bool
removeVideo token itemId =
  deleteRequest token $ baseUrl ++ "/playlistItems?id=" ++ T.unpack itemId

-- | Create a new playlist
createPlaylist :: TokenResponse -> T.Text -> T.Text -> T.Text -> IO (Maybe T.Text)
createPlaylist token title description privacy = do
  let url = baseUrl ++ "/playlists?part=snippet,status"
      reqBody = CreatePlaylistRequest
        { cpr_snippet = CreatePlaylistSnippet title description
        , cpr_status = CreatePlaylistStatus privacy
        }
  result <- postJSON token url reqBody :: IO (Either String CreatePlaylistResponse)
  case result of
    Left _ -> return Nothing
    Right resp -> return $ Just (cpr_id resp)

-- | Add video to playlist
addVideo :: TokenResponse -> T.Text -> T.Text -> IO Bool
addVideo token playlistId videoId = do
  let url = baseUrl ++ "/playlistItems?part=snippet"
      reqBody = AddVideoRequest
        { avr_snippet = AddVideoSnippet
            { avs_playlistId = playlistId
            , avs_resourceId = AddVideoResourceId "youtube#video" videoId
            }
        }
  result <- postJSON token url reqBody :: IO (Either String Value)
  return $ case result of Left _ -> False; Right _ -> True

-- | Delete a playlist
deletePlaylist :: TokenResponse -> T.Text -> IO Bool
deletePlaylist token playlistId =
  deleteRequest token $ baseUrl ++ "/playlists?id=" ++ T.unpack playlistId

-- | Find playlist item ID by video ID
findItemIdByVideoId :: TokenResponse -> T.Text -> T.Text -> IO (Maybe T.Text)
findItemIdByVideoId token playlistId videoId = do
  items <- fetchPlaylistItems token playlistId
  return $ plitem_id <$> find (\item -> res_videoId (plitem_resourceId $ plitem_snippet item) == videoId) items

-- | Remove video from playlist by video ID (looks up item ID automatically)
removeVideoByVideoId :: TokenResponse -> T.Text -> T.Text -> IO Bool
removeVideoByVideoId token playlistId videoId = do
  mbItemId <- findItemIdByVideoId token playlistId videoId
  case mbItemId of
    Nothing -> do
      putStrLn $ "Video not found in playlist: " ++ T.unpack videoId
      return False
    Just itemId -> removeVideo token itemId

-- | Add multiple videos to playlist from a list
addVideos :: TokenResponse -> T.Text -> [T.Text] -> IO (Int, Int)
addVideos token playlistId videoIds = go videoIds 0 0
  where
    go [] success failed = return (success, failed)
    go (vid:rest) success failed = do
      putStrLn $ "Adding: " ++ T.unpack vid
      result <- addVideo token playlistId vid
      if result
        then do
          putStrLn $ "  ✓ Added " ++ T.unpack vid
          go rest (success + 1) failed
        else do
          putStrLn $ "  ✗ Failed to add " ++ T.unpack vid
          go rest success (failed + 1)

-- | Remove multiple videos from playlist by video ID
removeVideosByVideoId :: TokenResponse -> T.Text -> [T.Text] -> IO (Int, Int)
removeVideosByVideoId token playlistId videoIds = go videoIds 0 0
  where
    go [] success failed = return (success, failed)
    go (vid:rest) success failed = do
      putStrLn $ "Removing: " ++ T.unpack vid
      result <- removeVideoByVideoId token playlistId vid
      if result
        then do
          putStrLn $ "  ✓ Removed " ++ T.unpack vid
          go rest (success + 1) failed
        else do
          putStrLn $ "  ✗ Failed to remove " ++ T.unpack vid
          go rest success (failed + 1)

-- | Move videos from one playlist to another
moveVideos :: TokenResponse -> T.Text -> T.Text -> [T.Text] -> IO (Int, Int, Int)
moveVideos token sourcePlaylist targetPlaylist videoIds = go videoIds 0 0 0
  where
    go [] added removed failed = return (added, removed, failed)
    go (vid:rest) added removed failed = do
      putStrLn $ "Moving: " ++ T.unpack vid
      -- Add to target
      addResult <- addVideo token targetPlaylist vid
      if addResult
        then do
          -- Remove from source
          removeResult <- removeVideoByVideoId token sourcePlaylist vid
          if removeResult
            then do
              putStrLn $ "  ✓ Moved " ++ T.unpack vid
              go rest (added + 1) (removed + 1) failed
            else do
              putStrLn $ "  ⚠ Added to target but failed to remove from source: " ++ T.unpack vid
              go rest (added + 1) removed (failed + 1)
        else do
          putStrLn $ "  ✗ Failed to add " ++ T.unpack vid
          go rest added removed (failed + 1)

-- =============================================================================
-- Main Program
-- =============================================================================

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["--help"] -> printUsage >> exitSuccess
    ["-h"] -> printUsage >> exitSuccess
    ["help"] -> printUsage >> exitSuccess

    ["auth"] -> do
      putStrLn "Authenticating with YouTube..."
      secrets <- loadClientSecrets
      _ <- authenticateInteractive (installed secrets)
      putStrLn "Authentication successful!"
      exitSuccess

    ["list-playlists"] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      listPlaylists token
      exitSuccess

    ["list", playlistId] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      listPlaylistVideos token (T.pack playlistId)
      exitSuccess

    ["list"] -> do
      putStrLn "Usage: ytplaylist list <playlist-id>"
      putStrLn "Example: ytplaylist list PLxxxxxxxxxxxxxx"
      exitFailure

    ["remove", itemId] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      success <- removeVideo token (T.pack itemId)
      if success
        then putStrLn $ "Removed: " ++ itemId
        else putStrLn "Failed to remove video"
      exitSuccess

    ["remove"] -> do
      putStrLn "Usage: ytplaylist remove <playlist-item-id>"
      putStrLn "Note: Use 'ytplaylist list <playlist-id>' to get item IDs"
      exitFailure

    ["create-playlist"] -> interactiveCreatePlaylist

    ["add-video", playlistId, videoId] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      success <- addVideo token (T.pack playlistId) (T.pack videoId)
      if success
        then putStrLn $ "Added video " ++ videoId ++ " to playlist"
        else putStrLn "Failed to add video"
      exitSuccess

    ["add-video", _] -> do
      putStrLn "Usage: ytplaylist add-video <playlist-id> <video-id>"
      putStrLn "Example: ytplaylist add-video PLxxxxxxxxxxxxxx dQw4w9WgXcQ"
      exitFailure

    ["delete-playlist", playlistId] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      success <- deletePlaylist token (T.pack playlistId)
      if success
        then putStrLn $ "Deleted playlist: " ++ playlistId
        else putStrLn "Failed to delete playlist"
      exitSuccess

    ["delete-playlist"] -> do
      putStrLn "Usage: ytplaylist delete-playlist <playlist-id>"
      exitFailure

    -- Batch operations
    ["add-videos", playlistId, filePath] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      content <- readFile filePath
      let videoIds = map T.pack $ filter (not . null) $ lines content
      putStrLn $ "Adding " ++ show (length videoIds) ++ " videos to playlist..."
      (success, failed) <- addVideos token (T.pack playlistId) videoIds
      putStrLn $ "\nDone! Added: " ++ show success ++ ", Failed: " ++ show failed
      exitSuccess

    ["add-videos", _] -> do
      putStrLn "Usage: ytplaylist add-videos <playlist-id> <file>"
      putStrLn "File should contain one video ID per line"
      exitFailure

    ["remove-videos", playlistId, filePath] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      content <- readFile filePath
      let videoIds = map T.pack $ filter (not . null) $ lines content
      putStrLn $ "Removing " ++ show (length videoIds) ++ " videos from playlist..."
      (success, failed) <- removeVideosByVideoId token (T.pack playlistId) videoIds
      putStrLn $ "\nDone! Removed: " ++ show success ++ ", Failed: " ++ show failed
      exitSuccess

    ["remove-videos", _] -> do
      putStrLn "Usage: ytplaylist remove-videos <playlist-id> <file>"
      putStrLn "File should contain one video ID per line"
      putStrLn "Videos are looked up by ID and removed from the playlist"
      exitFailure

    ["move-videos", sourcePlaylist, targetPlaylist, filePath] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      content <- readFile filePath
      let videoIds = map T.pack $ filter (not . null) $ lines content
      putStrLn $ "Moving " ++ show (length videoIds) ++ " videos..."
      (added, removed, failed) <- moveVideos token (T.pack sourcePlaylist) (T.pack targetPlaylist) videoIds
      putStrLn $ "\nDone! Added to target: " ++ show added ++ ", Removed from source: " ++ show removed ++ ", Failed: " ++ show failed
      exitSuccess

    ["move-videos", _] -> do
      putStrLn "Usage: ytplaylist move-videos <source-playlist-id> <target-playlist-id> <file>"
      putStrLn "File should contain one video ID per line"
      exitFailure

    [] -> interactiveMode

    _ -> printUsage >> exitFailure

interactiveCreatePlaylist :: IO ()
interactiveCreatePlaylist = do
  secrets <- loadClientSecrets
  token <- getOrRefreshToken (installed secrets)
  putStr "Playlist title: "
  hFlush stdout
  title <- T.getLine
  putStr "Description (optional): "
  hFlush stdout
  desc <- T.getLine
  putStr "Privacy (public/unlisted/private) [private]: "
  hFlush stdout
  privacy <- getLine
  let privacyStatus = if null privacy then "private" else privacy
  mbId <- createPlaylist token title desc (T.pack privacyStatus)
  case mbId of
    Just pid -> putStrLn $ "Created playlist: " ++ T.unpack pid
    Nothing -> putStrLn "Failed to create playlist"
  exitSuccess

interactiveMode :: IO ()
interactiveMode = do
  putStrLn "\n=== YouTube Playlist Manager ==="
  putStrLn "Run 'ytplaylist auth' first if not authenticated\n"
  loop
  where
    loop = do
      putStrLn "\nCommands:"
      putStrLn "  (p) list playlists"
      putStrLn "  (l) list videos in a playlist"
      putStrLn "  (r) remove video by item ID"
      putStrLn "  (c) create playlist"
      putStrLn "  (a) add video to playlist"
      putStrLn "  (d) delete playlist"
      putStrLn "  (q) quit"
      putStr "> "
      hFlush stdout
      cmd <- getLine
      case cmd of
        "p" -> cmdListPlaylists >> loop
        "l" -> cmdListVideos >> loop
        "r" -> cmdRemoveVideo >> loop
        "c" -> cmdCreatePlaylist >> loop
        "a" -> cmdAddVideo >> loop
        "d" -> cmdDeletePlaylist >> loop
        "q" -> putStrLn "Goodbye!"
        _ -> putStrLn "Unknown command" >> loop

cmdListPlaylists :: IO ()
cmdListPlaylists = do
  result <- try @SomeException $ do
    secrets <- loadClientSecrets
    token <- getOrRefreshToken (installed secrets)
    listPlaylists token
  case result of
    Left e -> putStrLn $ "Error: " ++ show e
    Right () -> return ()

cmdListVideos :: IO ()
cmdListVideos = do
  putStr "Enter playlist ID: "
  hFlush stdout
  pid <- T.getLine
  result <- try @SomeException $ do
    secrets <- loadClientSecrets
    token <- getOrRefreshToken (installed secrets)
    listPlaylistVideos token pid
  case result of
    Left e -> putStrLn $ "Error: " ++ show e
    Right () -> return ()

cmdRemoveVideo :: IO ()
cmdRemoveVideo = do
  putStr "Enter playlist item ID to remove: "
  hFlush stdout
  itemId <- T.getLine
  result <- try @SomeException $ do
    secrets <- loadClientSecrets
    token <- getOrRefreshToken (installed secrets)
    success <- removeVideo token itemId
    putStrLn $ if success then "Removed successfully" else "Failed to remove"
  case result of
    Left e -> putStrLn $ "Error: " ++ show e
    Right () -> return ()

cmdCreatePlaylist :: IO ()
cmdCreatePlaylist = do
  putStr "Playlist title: "
  hFlush stdout
  title <- T.getLine
  putStr "Description: "
  hFlush stdout
  desc <- T.getLine
  putStr "Privacy (public/unlisted/private): "
  hFlush stdout
  privacy <- T.getLine
  result <- try @SomeException $ do
    secrets <- loadClientSecrets
    token <- getOrRefreshToken (installed secrets)
    mbId <- createPlaylist token title desc privacy
    case mbId of
      Just pid -> putStrLn $ "Created: " ++ T.unpack pid
      Nothing -> putStrLn "Failed to create playlist"
  case result of
    Left e -> putStrLn $ "Error: " ++ show e
    Right () -> return ()

cmdAddVideo :: IO ()
cmdAddVideo = do
  putStr "Playlist ID: "
  hFlush stdout
  pid <- T.getLine
  putStr "Video ID: "
  hFlush stdout
  vid <- T.getLine
  result <- try @SomeException $ do
    secrets <- loadClientSecrets
    token <- getOrRefreshToken (installed secrets)
    success <- addVideo token pid vid
    putStrLn $ if success then "Added successfully" else "Failed to add"
  case result of
    Left e -> putStrLn $ "Error: " ++ show e
    Right () -> return ()

cmdDeletePlaylist :: IO ()
cmdDeletePlaylist = do
  putStr "Playlist ID to delete: "
  hFlush stdout
  pid <- T.getLine
  putStr "Are you sure? (yes/no): "
  hFlush stdout
  confirm <- getLine
  if confirm /= "yes"
    then putStrLn "Cancelled"
    else do
      result <- try @SomeException $ do
        secrets <- loadClientSecrets
        token <- getOrRefreshToken (installed secrets)
        success <- deletePlaylist token pid
        putStrLn $ if success then "Deleted successfully" else "Failed to delete"
      case result of
        Left e -> putStrLn $ "Error: " ++ show e
        Right () -> return ()

printUsage :: IO ()
printUsage = do
  putStrLn "YouTube Playlist Manager - Manage YouTube playlists via API"
  putStrLn ""
  putStrLn "Usage: ytplaylist [COMMAND]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  auth                              Authenticate with YouTube"
  putStrLn "  list-playlists                    List all your playlists"
  putStrLn "  list <playlist-id>                List all videos in a playlist"
  putStrLn "  remove <playlist-item-id>         Remove video from playlist (by item ID)"
  putStrLn "  create-playlist                   Create a new playlist (interactive)"
  putStrLn "  add-video <playlist-id> <vid>     Add video to playlist"
  putStrLn "  delete-playlist <playlist-id>     Delete a playlist"
  putStrLn ""
  putStrLn "Batch Operations:"
  putStrLn "  add-videos <playlist> <file>      Add multiple videos from file (1 ID/line)"
  putStrLn "  remove-videos <playlist> <file>   Remove videos by video ID (looks up item ID)"
  putStrLn "  move-videos <src> <dst> <file>    Move videos between playlists"
  putStrLn ""
  putStrLn "  (no command)                      Interactive mode"
  putStrLn "  --help, -h, help                  Show this help"
  putStrLn ""
  putStrLn "Setup:"
  putStrLn "  1. Place client_secrets.json in this directory"
  putStrLn "  2. Run: ytplaylist auth"
  putStrLn "  3. Run: ytplaylist"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  ytplaylist list-playlists                    # Show all playlists"
  putStrLn "  ytplaylist list PLxxxxxxxxxxx                # List videos"
  putStrLn "  ytplaylist remove PLxxxxxxxxxxx.xxxxxxxxxxx  # Remove video"
  putStrLn "  ytplaylist add-videos PLxxx videos.txt       # Batch add"
  putStrLn "  ytplaylist remove-videos PLxxx videos.txt    # Batch remove by video ID"
  putStrLn "  ytplaylist move-videos PLsrc PLdst vids.txt  # Batch move"
