{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module YouTube
  ( -- Playlist operations
    listPlaylists
  , createPlaylist
  , deletePlaylist
    -- Playlist item operations
  , fetchPlaylistItems
  , listPlaylistVideos
  , addVideo
  , addVideos
  , removeVideo
  , removeVideosByItemId
  , removeVideosByVideoId
  , removeVideoByVideoId
  , moveVideos
    -- Lookup helpers
  , findItemIdByVideoId
  ) where

import Data.Aeson (Value)
import qualified Data.Text as T
import Data.List (find, sortBy)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Network.URI.Encode as URI
import Fmt (fmtLn, (+||), (||+), build, padLeftF, padRightF)

-- Note: We import specific items from HTTP to ensure future edits don't accidentally
-- break the build. If you add new functions to HTTP.hs, you must explicitly add them
-- to this import list. This prevents issues like only importing BatchResult and losing
-- access to baseUrl, getJSON, postJSON, deleteRequest, etc.
import Network.OAuth.OAuth2 (OAuth2Token(..))
import Types
import HTTP
  ( BatchResult(..)
  , baseUrl
  , getJSON
  , postJSON
  , deleteRequest
  )

-- =============================================================================
-- Playlist Operations
-- =============================================================================

listPlaylists :: OAuth2Token -> IO ()
listPlaylists token = do
  let url = baseUrl ++ "/playlists?part=snippet,contentDetails&mine=true&maxResults=50"
  result <- getJSON (accessToken token) url :: IO (Either String PlaylistsResponse)
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ err
    Right resp -> do
      fmtLn "\n=== Your Playlists ===\n"
      mapM_ printPlaylist (pr_items resp)
      fmtLn $ "\nTotal: "+||length (pr_items resp)||+" playlists"
  where
    printPlaylist :: PlaylistInfo -> IO ()
    printPlaylist pl = do
      fmtLn $ ""+||(plsi_title $ pli_snippet pl)||+""
      fmtLn $ "  ID: "+||(pli_id pl)||+""
      fmtLn $ "  Videos: "+||(plcd_itemCount $ pli_contentDetails pl)||+""
      fmtLn ""

createPlaylist :: OAuth2Token -> T.Text -> T.Text -> T.Text -> IO (Maybe T.Text)
createPlaylist token title description privacy = do
  let url = baseUrl ++ "/playlists?part=snippet,status"
      reqBody = CreatePlaylistRequest
        { cpr_snippet = CreatePlaylistSnippet title description
        , cpr_status = CreatePlaylistStatus privacy
        }
  result <- postJSON (accessToken token) url reqBody :: IO (Either String CreatePlaylistResponse)
  case result of
    Left _ -> return Nothing
    Right resp -> return $ Just (cpr_id resp)

deletePlaylist :: OAuth2Token -> T.Text -> IO Bool
deletePlaylist token playlistId =
  deleteRequest (accessToken token) $ baseUrl ++ "/playlists?id=" ++ T.unpack playlistId

-- =============================================================================
-- Playlist Item Operations
-- =============================================================================

fetchPlaylistItems :: OAuth2Token -> T.Text -> IO [PlaylistItem]
fetchPlaylistItems token playlistId = fetchPages Nothing
  where
    fetchPages pageToken = do
      let base = baseUrl ++ "/playlistItems?part=snippet&playlistId=" ++ T.unpack playlistId ++ "&maxResults=50"
          url = case pageToken of
                  Nothing -> base
                  Just tok -> base ++ "&pageToken=" ++ URI.encode (T.unpack tok)
      result <- getJSON (accessToken token) url :: IO (Either String PlaylistItemsResponse)
      case result of
        Left _ -> return []
        Right resp -> do
          rest <- case pir_nextPageToken resp of
            Nothing -> return []
            Just nextTok -> fetchPages (Just nextTok)
          return $ pir_items resp ++ rest

listPlaylistVideos :: OAuth2Token -> T.Text -> IO ()
listPlaylistVideos token playlistId = do
  items <- fetchPlaylistItems token playlistId
  fmtLn "\n=== Playlist Videos ==="
  fmtLn $ "Playlist ID: "+||playlistId||+""
  fmtLn $ "Total: "+||length items||+" videos\n"
  fmtLn $ padLeftF 4 ' ' (T.pack "#")<>build (T.pack "  ")<>padRightF 20 ' ' (T.pack "Item ID")<>build (T.pack "  ")<>padRightF 15 ' ' (T.pack "Video ID")<>build (T.pack "  Title")
  fmtLn $ build $ T.replicate 90 "-"
  mapM_ printItem (zip [1..] items)
  where
    printItem (idx, item) = do
      let num = padLeftF 4 ' ' $ show (idx :: Int)
          itemId = padRightF 20 ' ' $ T.unpack $ plitem_id item
          vidId = padRightF 15 ' ' $ T.unpack $ res_videoId $ plitem_resourceId $ plitem_snippet item
          titleStr = T.unpack $ plitem_title $ plitem_snippet item
          title = if length titleStr > 40 then take 37 titleStr ++ "..." else titleStr
      fmtLn $ num<>build (T.pack "  ")<>itemId<>build (T.pack "  ")<>vidId<>build (T.pack "  ")<>build title

addVideo :: OAuth2Token -> T.Text -> T.Text -> IO Bool
addVideo token playlistId videoId = do
  let url = baseUrl ++ "/playlistItems?part=snippet"
      reqBody = AddVideoRequest
        { avr_snippet = AddVideoSnippet
            { avs_playlistId = playlistId
            , avs_resourceId = AddVideoResourceId "youtube#video" videoId
            }
        }
  result <- postJSON (accessToken token) url reqBody :: IO (Either String Value)
  return $ case result of Left _ -> False; Right _ -> True

addVideos :: OAuth2Token -> T.Text -> [T.Text] -> IO ([T.Text], [T.Text])
addVideos token playlistId videoIds = do
  let total = length videoIds
  fmtLn $ "Adding "+||total||+" videos one at a time..."
  results <- mapM (addVideoSingle token playlistId) videoIds
  let successIds = map fst $ filter snd $ zip videoIds results
      failedIds = map fst $ filter (not . snd) $ zip videoIds results
  return (successIds, failedIds)

addVideoSingle :: OAuth2Token -> T.Text -> T.Text -> IO Bool
addVideoSingle token playlistId videoId = do
  success <- addVideo token playlistId videoId
  if success
    then fmtLn $ "  ✓ "+||videoId||+""
    else fmtLn $ "  ✗ "+||videoId||+""
  return success

removeVideo :: OAuth2Token -> T.Text -> IO Bool
removeVideo token itemId =
  deleteRequest (accessToken token) $ baseUrl ++ "/playlistItems?id=" ++ T.unpack itemId

removeVideosByItemId :: OAuth2Token -> [PlaylistItem] -> IO [Bool]
removeVideosByItemId token items = do
  let total = length items
  fmtLn $ "Removing "+||total||+" videos one at a time..."
  results <- mapM removeSingle items
  return $ map batchSuccess results
  where
    removeSingle item = do
      let vid = res_videoId $ plitem_resourceId $ plitem_snippet item
      success <- removeVideo token (plitem_id item)
      if success
        then fmtLn $ "  ✓ "+||vid||+""
        else fmtLn $ "  ✗ "+||vid||+""
      return $ BatchResult success (if success then Nothing else Just "Failed to remove")

removeVideosByVideoId :: OAuth2Token -> T.Text -> [T.Text] -> IO (Int, Int)
removeVideosByVideoId token playlistId videoIds = do
  let videoIdSet = Map.fromList $ map (, ()) videoIds
  fmtLn $ "Fetching playlist to find "+||length videoIds||+" videos..."
  items <- fetchPlaylistItems token playlistId
  let itemsToRemove = filter shouldRemove items
      shouldRemove item = res_videoId (plitem_resourceId $ plitem_snippet item) `Map.member` videoIdSet
  results <- removeVideosByItemId token itemsToRemove
  let success = length $ filter id results
      failed = length videoIds - success
  return (success, failed)

removeVideoByVideoId :: OAuth2Token -> T.Text -> T.Text -> IO Bool
removeVideoByVideoId token playlistId videoId = do
  mbItemId <- findItemIdByVideoId token playlistId videoId
  case mbItemId of
    Nothing -> do
      fmtLn $ "Video not found in playlist: "+||videoId||+""
      return False
    Just itemId -> removeVideo token itemId

moveVideos :: OAuth2Token -> T.Text -> T.Text -> [T.Text] -> IO (Int, Int, Int)
moveVideos token sourcePlaylist targetPlaylist videoIds = do
  let total = length videoIds

  fmtLn $ "Phase 1: Adding "+||total||+" videos to target playlist..."
  (addedIds, failedIds) <- addVideos token targetPlaylist videoIds
  let added = length addedIds

  if null addedIds
    then do
      fmtLn "\nNo videos were successfully added. Skipping removal phase."
      return (0, 0, total)
    else do
      fmtLn "\nPhase 2: Fetching source playlist for removal..."
      let addedSet = Map.fromList $ map (, ()) addedIds
      items <- fetchPlaylistItems token sourcePlaylist
      let itemsToRemove = sortByPositionDesc $ filter shouldRemove items
          shouldRemove item = res_videoId (plitem_resourceId $ plitem_snippet item) `Map.member` addedSet

      removeResults <- removeVideosByItemId token itemsToRemove

      let removed = length $ filter id removeResults
          failed = length failedIds

      return (added, removed, failed)
  where
    sortByPositionDesc = sortBy (flip compare `on` (plitem_position . plitem_snippet))

-- =============================================================================
-- Lookup Helpers
-- =============================================================================

findItemIdByVideoId :: OAuth2Token -> T.Text -> T.Text -> IO (Maybe T.Text)
findItemIdByVideoId token playlistId videoId = do
  items <- fetchPlaylistItems token playlistId
  return $ plitem_id <$> find (\item -> res_videoId (plitem_resourceId $ plitem_snippet item) == videoId) items
