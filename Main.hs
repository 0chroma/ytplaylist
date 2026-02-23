{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Text as T
import Options.Applicative
import System.IO (hSetBuffering, stdout, stderr, BufferMode(..))

import OAuth
import YouTube

-- =============================================================================
-- Command Types
-- =============================================================================

data Command
  = Auth
  | ListPlaylists
  | ListVideos T.Text
  | RemoveVideo T.Text T.Text
  | CreatePlaylist T.Text T.Text T.Text
  | AddVideo T.Text T.Text
  | AddVideoBatch T.Text FilePath
  | DeletePlaylist T.Text
  | RemoveBatch T.Text FilePath
  | MoveVideo T.Text T.Text T.Text
  | MoveBatch T.Text T.Text FilePath

-- =============================================================================
-- Main
-- =============================================================================

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cmd <- execParser opts
  runCommand cmd
  where
    opts = info (commandParser <**> helper) $
      fullDesc <> header "YouTube Playlist Manager - Manage YouTube playlists via API"

-- =============================================================================
-- Command Parser
-- =============================================================================

commandParser :: Parser Command
commandParser = hsubparser
  ( command "auth" (info (pure Auth) $ progDesc "Authenticate with YouTube")
  <> command "list-playlists" (info (pure ListPlaylists) $ progDesc "List all your playlists")
  <> command "list" (info listVideosParser $ progDesc "List all videos in a playlist")
  <> command "remove" (info removeVideoParser $ progDesc "Remove video from playlist by video ID")
  <> command "create-playlist" (info createPlaylistParser $ progDesc "Create a new playlist")
  <> command "add" (info addVideoParser $ progDesc "Add video to playlist")
  <> command "add-batch" (info addVideoBatchParser $ progDesc "Add multiple videos from file (1 ID/line)")
  <> command "delete-playlist" (info deletePlaylistParser $ progDesc "Delete a playlist")
  <> command "remove-batch" (info removeBatchParser $ progDesc "Remove videos by video ID from file (1 ID/line)")
  <> command "move" (info moveVideoParser $ progDesc "Move video between playlists")
  <> command "move-batch" (info moveBatchParser $ progDesc "Move videos between playlists from file (1 ID/line)")
  )

listVideosParser :: Parser Command
listVideosParser = ListVideos . T.pack <$> argument str (metavar "PLAYLIST-ID")

removeVideoParser :: Parser Command
removeVideoParser = RemoveVideo
  <$> (T.pack <$> argument str (metavar "PLAYLIST-ID"))
  <*> (T.pack <$> argument str (metavar "VIDEO-ID"))

createPlaylistParser :: Parser Command
createPlaylistParser = CreatePlaylist
  <$> (T.pack <$> argument str (metavar "TITLE"))
  <*> (T.pack <$> argument str (metavar "DESCRIPTION"))
  <*> (T.pack <$> argument str (metavar "PRIVACY"))

addVideoParser :: Parser Command
addVideoParser = AddVideo
  <$> (T.pack <$> argument str (metavar "PLAYLIST-ID"))
  <*> (T.pack <$> argument str (metavar "VIDEO-ID"))

addVideoBatchParser :: Parser Command
addVideoBatchParser = AddVideoBatch
  <$> (T.pack <$> argument str (metavar "PLAYLIST-ID"))
  <*> argument str (metavar "FILE")

deletePlaylistParser :: Parser Command
deletePlaylistParser = DeletePlaylist . T.pack <$> argument str (metavar "PLAYLIST-ID")

removeBatchParser :: Parser Command
removeBatchParser = RemoveBatch
  <$> (T.pack <$> argument str (metavar "PLAYLIST-ID"))
  <*> argument str (metavar "FILE")

moveVideoParser :: Parser Command
moveVideoParser = MoveVideo
  <$> (T.pack <$> argument str (metavar "SOURCE-PLAYLIST-ID"))
  <*> (T.pack <$> argument str (metavar "TARGET-PLAYLIST-ID"))
  <*> (T.pack <$> argument str (metavar "VIDEO-ID"))

moveBatchParser :: Parser Command
moveBatchParser = MoveBatch
  <$> (T.pack <$> argument str (metavar "SOURCE-PLAYLIST-ID"))
  <*> (T.pack <$> argument str (metavar "TARGET-PLAYLIST-ID"))
  <*> argument str (metavar "FILE")

-- =============================================================================
-- Token Helper
-- =============================================================================

withToken :: (OAuth2Token -> IO a) -> IO a
withToken run = do
  oauth2 <- loadClientSecrets
  token <- getOrRefreshToken oauth2
  run token

-- =============================================================================
-- Command Execution
-- =============================================================================

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  Auth -> do
    putStrLn "Authenticating with YouTube..."
    oauth2 <- loadClientSecrets
    _ <- authenticateInteractive oauth2
    putStrLn "Authentication successful!"

  ListPlaylists -> withToken listPlaylists

  ListVideos pid -> withToken (`listPlaylistVideos` pid)

  RemoveVideo playlistId videoId -> withToken $ \token -> do
    success <- removeVideoByVideoId token playlistId videoId
    putStrLn $ if success
      then "Removed video " ++ T.unpack videoId ++ " from playlist"
      else "Failed to remove video"

  CreatePlaylist title desc privacy -> withToken $ \token -> do
    mbId <- createPlaylist token title desc privacy
    case mbId of
      Just pid -> putStrLn $ T.unpack pid
      Nothing -> putStrLn "Failed to create playlist"

  AddVideo pid vid -> withToken $ \token -> do
    success <- addVideo token pid vid
    putStrLn $ if success
      then "Added video " ++ T.unpack vid ++ " to playlist"
      else "Failed to add video"

  AddVideoBatch pid file -> do
    videoIds <- readVideoIds file
    putStrLn $ "Adding " ++ show (length videoIds) ++ " videos to playlist..."
    withToken $ \token -> do
      (successIds, failedIds) <- addVideos token pid videoIds
      putStrLn $ "\nDone! Added: " ++ show (length successIds) ++ ", Failed: " ++ show (length failedIds)

  DeletePlaylist pid -> withToken $ \token -> do
    success <- deletePlaylist token pid
    putStrLn $ if success then "Deleted playlist: " ++ T.unpack pid else "Failed to delete playlist"

  RemoveBatch pid file -> do
    videoIds <- readVideoIds file
    putStrLn $ "Removing " ++ show (length videoIds) ++ " videos from playlist..."
    withToken $ \token -> do
      (success, failed) <- removeVideosByVideoId token pid videoIds
      putStrLn $ "\nDone! Removed: " ++ show success ++ ", Failed: " ++ show failed

  MoveVideo src dst vid -> do
    putStrLn $ "Moving video " ++ T.unpack vid ++ "..."
    withToken $ \token -> do
      success <- addVideo token dst vid
      if success
        then do
          removed <- removeVideoByVideoId token src vid
          if removed
            then putStrLn "Done! Video moved successfully"
            else putStrLn "Warning: Added to target but failed to remove from source"
        else putStrLn "Failed to add video to target playlist"

  MoveBatch src dst file -> do
    videoIds <- readVideoIds file
    putStrLn $ "Moving " ++ show (length videoIds) ++ " videos..."
    withToken $ \token -> do
      (added, removed, failed) <- moveVideos token src dst videoIds
      putStrLn $ "\nDone! Added: " ++ show added ++ ", Removed: " ++ show removed ++ ", Failed: " ++ show failed

readVideoIds :: FilePath -> IO [T.Text]
readVideoIds file = do
  content <- readFile file
  return $ map T.pack $ filter (not . null) $ lines content
