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
  , fetchVideoDurations
  ) where

import Data.Aeson (Value)
import qualified Data.Text as T
import Data.List (find, sortBy)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Network.URI.Encode as URI
import Fmt (fmtLn, (+||), (||+), build, padLeftF, padRightF)

import Network.OAuth.OAuth2 (OAuth2Token(..))
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
      let base = baseUrl ++ "/playlistItems?part=snippet,contentDetails&playlistId=" ++ T.unpack playlistId ++ "&maxResults=50"
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
  let videoIds = map (res_videoId . plitem_resourceId . plitem_snippet) items
  durations <- fetchVideoDurations token videoIds
  fmtLn "\n=== Playlist Videos ==="
  fmtLn $ "Playlist ID: "+||playlistId||+""
  fmtLn $ "Total: "+||length items||+" videos\n"
  fmtLn $ padLeftF 4 ' ' (T.pack "#")<>build (T.pack "  ")<>padRightF 8 ' ' (T.pack "Duration")<>padRightF 15 ' ' (T.pack "Video ID")<>build (T.pack "  Title")
  fmtLn $ build $ T.replicate 75 "-"
  mapM_ (printItem durations) (zip [1..] items)
  where
    printItem durations (idx, item) = do
      let num = padLeftF 4 ' ' $ show (idx :: Int)
          vidId = res_videoId $ plitem_resourceId $ plitem_snippet item
          duration = case Map.lookup vidId durations of
                      Just d -> padRightF 8 ' ' $ T.unpack d
                      Nothing -> padRightF 8 ' ' $ T.unpack (T.pack "??:??:??")
          titleStr = T.unpack $ plitem_title $ plitem_snippet item
          title = if length titleStr > 50 then take 47 titleStr ++ "..." else titleStr
      fmtLn $ num<>build (T.pack "  ")<>duration<>build (T.pack "  ")<>build (T.unpack vidId)<>build (T.pack "  ")<>build title

fetchVideoDurations :: OAuth2Token -> [T.Text] -> IO (Map.Map T.Text T.Text)
fetchVideoDurations token videoIds = do
  let batches = chunksOf 50 videoIds
  results <- mapM fetchBatch batches
  return $ Map.fromList $ concat results
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
    fetchBatch ids = do
      let idsParam = T.intercalate "," ids
          url = baseUrl ++ "/videos?part=contentDetails&id=" ++ T.unpack idsParam
      result <- getJSON (accessToken token) url :: IO (Either String VideosResponse)
      case result of
        Left _ -> return []
        Right resp -> return $ map extractDuration $ vr_items resp
    extractDuration item = (vi_id item, parseDuration $ vcd_duration $ vi_contentDetails item)
    parseDuration :: T.Text -> T.Text
    parseDuration dur
      | T.null dur = "??:??:??"
      | otherwise = formatDuration dur
    formatDuration d = formatFromISO d
      where
        formatFromISO iso
          | "PT" `T.isPrefixOf` iso = formatPT (T.drop 2 iso)
          | otherwise = iso
        formatPT pt
          | "H" `T.isInfixOf` pt = formatWithHours pt
          | otherwise = formatMinutesOnly pt
        formatWithHours pt = 
          let (hPart, rest1) = T.breakOn "H" pt
              afterH = T.drop 1 rest1
              (mPart, rest2) = T.breakOn "M" afterH
              hasM = "M" `T.isInfixOf` afterH
              sPart = if hasM 
                      then T.takeWhile (/= 'S') (T.drop 1 rest2)
                      else T.takeWhile (/= 'S') afterH
              h = if T.null hPart then "0" else T.unpack hPart
              m = if T.null mPart || not hasM then "00" else padNum $ T.unpack mPart
              s = if T.null sPart then "00" else padNum $ T.unpack sPart
          in T.pack $ h ++ ":" ++ m ++ ":" ++ s
        formatMinutesOnly pt =
          let (mPart, rest) = T.breakOn "M" pt
              sPart = T.takeWhile (/= 'S') (T.drop 1 rest)
              m = if T.null mPart then "00" else padNum $ T.unpack mPart
              s = if T.null sPart then "00" else padNum $ T.unpack sPart
          in T.pack $ m ++ ":" ++ s
        padNum n = case length n of
                     1 -> "0" ++ n
                     _ -> n

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
      batchSize = 100
      batches = chunksOf batchSize videoIds
  fmtLn $ "Adding "+||total||+" videos in "+||length batches||+" batch(es)..."
  results <- concat <$> mapM (addBatch token playlistId) (zip [1..] batches)
  let successIds = map fst $ filter (batchSuccess . snd) $ zip videoIds results
      failedIds = map fst $ filter (not . batchSuccess . snd) $ zip videoIds results
  return (successIds, failedIds)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

addBatch :: OAuth2Token -> T.Text -> (Int, [T.Text]) -> IO [BatchResult]
addBatch token playlistId (batchNum, videoIds) = do
  fmtLn $ "  Batch "+||batchNum||+": "+||length videoIds||+" videos"
  let subRequests = map (buildAddSubRequest playlistId) videoIds
  results <- batchRequest (accessToken token) subRequests
  mapM_ printResult (zip videoIds results)
  return results
  where
    printResult (vid, result) =
      case batchError result of
        Just err -> fmtLn $ "    ✗ "+||vid||+" ("+||err||+")"
        Nothing -> fmtLn $ if batchSuccess result then "    ✓ "+||vid||+"" else "    ✗ "+||vid||+""

removeVideo :: OAuth2Token -> T.Text -> IO Bool
removeVideo token itemId =
  deleteRequest (accessToken token) $ baseUrl ++ "/playlistItems?id=" ++ T.unpack itemId

removeVideosByItemId :: OAuth2Token -> [PlaylistItem] -> IO [Bool]
removeVideosByItemId token items = do
  let total = length items
      batchSize = 100
      batches = chunksOf batchSize items
  fmtLn $ "Removing "+||total||+" videos in "+||length batches||+" batch(es)..."
  results <- concat <$> mapM removeBatch (zip [1::Int ..] batches)
  return $ map batchSuccess results
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
    removeBatch (batchNum, batchItems) = do
      fmtLn $ "  Batch "+||batchNum||+": "+||length batchItems||+" videos"
      let subRequests = map (buildDeleteSubRequest . plitem_id) batchItems
      results <- batchRequest (accessToken token) subRequests
      mapM_ printResult (zip batchItems results)
      return results
    printResult (item, result) = do
      let vid = res_videoId $ plitem_resourceId $ plitem_snippet item
      case batchError result of
        Just err -> fmtLn $ "    ✗ "+||vid||+" ("+||err||+")"
        Nothing -> fmtLn $ if batchSuccess result then "    ✓ "+||vid||+"" else "    ✗ "+||vid||+""

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
      let itemsToRemove = reverse $ sortByPosition $ filter shouldRemove items
          shouldRemove item = res_videoId (plitem_resourceId $ plitem_snippet item) `Map.member` addedSet

      removeResults <- removeVideosByItemId token itemsToRemove

      let removed = length $ filter id removeResults
          failed = length failedIds

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
