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

import Network.OAuth.OAuth2 (AccessToken(..), OAuth2Token(..))
import Types
import HTTP

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
      putStrLn "\n=== Your Playlists ===\n"
      mapM_ printPlaylist (pr_items resp)
      putStrLn $ "\nTotal: " ++ show (length $ pr_items resp) ++ " playlists"
  where
    printPlaylist pl = do
      putStrLn $ T.unpack (plsi_title $ pli_snippet pl)
      putStrLn $ "  ID: " ++ T.unpack (pli_id pl)
      putStrLn $ "  Videos: " ++ show (plcd_itemCount $ pli_contentDetails pl)
      putStrLn ""

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

addVideos :: OAuth2Token -> T.Text -> [T.Text] -> IO (Int, Int)
addVideos token playlistId videoIds = do
  let total = length videoIds
      batchSize = 100
      batches = chunksOf batchSize videoIds
  putStrLn $ "Adding " ++ show total ++ " videos in " ++ show (length batches) ++ " batch(es)..."
  results <- concat <$> mapM (addBatch token playlistId) (zip [1..] batches)
  let success = length $ filter id results
      failed = total - success
  return (success, failed)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

addBatch :: OAuth2Token -> T.Text -> (Int, [T.Text]) -> IO [Bool]
addBatch token playlistId (batchNum, videoIds) = do
  putStrLn $ "  Batch " ++ show batchNum ++ ": " ++ show (length videoIds) ++ " videos"
  let subRequests = map (buildAddSubRequest playlistId) videoIds
  results <- batchRequest (accessToken token) subRequests
  mapM_ printResult (zip videoIds results)
  return results
  where
    printResult (vid, success) =
      putStrLn $ if success then "    ✓ " ++ T.unpack vid else "    ✗ " ++ T.unpack vid

removeVideo :: OAuth2Token -> T.Text -> IO Bool
removeVideo token itemId =
  deleteRequest (accessToken token) $ baseUrl ++ "/playlistItems?id=" ++ T.unpack itemId

removeVideosByItemId :: OAuth2Token -> [PlaylistItem] -> IO [Bool]
removeVideosByItemId token items = do
  let total = length items
      batchSize = 100
      batches = chunksOf batchSize items
  putStrLn $ "Removing " ++ show total ++ " videos in " ++ show (length batches) ++ " batch(es)..."
  results <- concat <$> mapM removeBatch (zip [1..] batches)
  return results
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
    removeBatch (batchNum, batchItems) = do
      putStrLn $ "  Batch " ++ show batchNum ++ ": " ++ show (length batchItems) ++ " videos"
      let subRequests = map (buildDeleteSubRequest . plitem_id) batchItems
      results <- batchRequest (accessToken token) subRequests
      mapM_ printResult (zip batchItems results)
      return results
    printResult (item, success) = do
      let vid = res_videoId $ plitem_resourceId $ plitem_snippet item
      putStrLn $ if success then "    ✓ " ++ T.unpack vid else "    ✗ " ++ T.unpack vid

removeVideosByVideoId :: OAuth2Token -> T.Text -> [T.Text] -> IO (Int, Int)
removeVideosByVideoId token playlistId videoIds = do
  let videoIdSet = Map.fromList $ map (, ()) videoIds
  putStrLn $ "Fetching playlist to find " ++ show (length videoIds) ++ " videos..."
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
      putStrLn $ "Video not found in playlist: " ++ T.unpack videoId
      return False
    Just itemId -> removeVideo token itemId

moveVideos :: OAuth2Token -> T.Text -> T.Text -> [T.Text] -> IO (Int, Int, Int)
moveVideos token sourcePlaylist targetPlaylist videoIds = do
  let total = length videoIds
      videoIdSet = Map.fromList $ map (, ()) videoIds

  putStrLn $ "Phase 1: Adding " ++ show total ++ " videos to target playlist..."
  (added, _) <- addVideos token targetPlaylist videoIds

  putStrLn $ "\nPhase 2: Fetching source playlist for removal..."
  items <- fetchPlaylistItems token sourcePlaylist
  let itemsToRemove = reverse $ sortByPosition $ filter shouldRemove items
      shouldRemove item = res_videoId (plitem_resourceId $ plitem_snippet item) `Map.member` videoIdSet

  removeResults <- removeVideosByItemId token itemsToRemove

  let removed = length $ filter id removeResults
      failed = total - added

  return (added, removed, failed)
  where
    sortByPosition = sortBy (compare `on` (plitem_position . plitem_snippet))

-- =============================================================================
-- Lookup Helpers
-- =============================================================================

findItemIdByVideoId :: OAuth2Token -> T.Text -> T.Text -> IO (Maybe T.Text)
findItemIdByVideoId token playlistId videoId = do
  items <- fetchPlaylistItems token playlistId
  return $ plitem_id <$> find (\item -> res_videoId (plitem_resourceId $ plitem_snippet item) == videoId) items
