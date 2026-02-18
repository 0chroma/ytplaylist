{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout, stderr, hSetBuffering, BufferMode(..))
import Control.Exception (try, SomeException)

import OAuth
import YouTube

-- =============================================================================
-- Command Types
-- =============================================================================

data Command
  = Auth
  | ListPlaylists
  | ListVideos T.Text
  | RemoveVideo T.Text
  | CreatePlaylist (Maybe T.Text) (Maybe T.Text) (Maybe T.Text)
  | AddVideo T.Text T.Text
  | DeletePlaylist T.Text
  | AddVideos T.Text FilePath
  | RemoveVideos T.Text FilePath
  | MoveVideos T.Text T.Text FilePath
  | Interactive

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
  <> command "remove" (info removeVideoParser $ progDesc "Remove video from playlist (by item ID)")
  <> command "create-playlist" (info createPlaylistParser $ progDesc "Create a new playlist")
  <> command "add-video" (info addVideoParser $ progDesc "Add video to playlist")
  <> command "delete-playlist" (info deletePlaylistParser $ progDesc "Delete a playlist")
  <> command "add-videos" (info addVideosParser $ progDesc "Add multiple videos from file (1 ID/line)")
  <> command "remove-videos" (info removeVideosParser $ progDesc "Remove videos by video ID")
  <> command "move-videos" (info moveVideosParser $ progDesc "Move videos between playlists")
  ) <|> pure Interactive

listVideosParser :: Parser Command
listVideosParser = ListVideos . T.pack <$> argument str (metavar "PLAYLIST-ID")

removeVideoParser :: Parser Command
removeVideoParser = RemoveVideo . T.pack <$> argument str (metavar "PLAYLIST-ITEM-ID")

createPlaylistParser :: Parser Command
createPlaylistParser = CreatePlaylist
  <$> optional (T.pack <$> argument str (metavar "TITLE"))
  <*> optional (T.pack <$> argument str (metavar "DESCRIPTION"))
  <*> optional (T.pack <$> argument str (metavar "PRIVACY"))

addVideoParser :: Parser Command
addVideoParser = AddVideo
  <$> (T.pack <$> argument str (metavar "PLAYLIST-ID"))
  <*> (T.pack <$> argument str (metavar "VIDEO-ID"))

deletePlaylistParser :: Parser Command
deletePlaylistParser = DeletePlaylist . T.pack <$> argument str (metavar "PLAYLIST-ID")

addVideosParser :: Parser Command
addVideosParser = AddVideos
  <$> (T.pack <$> argument str (metavar "PLAYLIST-ID"))
  <*> argument str (metavar "FILE")

removeVideosParser :: Parser Command
removeVideosParser = RemoveVideos
  <$> (T.pack <$> argument str (metavar "PLAYLIST-ID"))
  <*> argument str (metavar "FILE")

moveVideosParser :: Parser Command
moveVideosParser = MoveVideos
  <$> (T.pack <$> argument str (metavar "SOURCE-PLAYLIST-ID"))
  <*> (T.pack <$> argument str (metavar "TARGET-PLAYLIST-ID"))
  <*> argument str (metavar "FILE")

-- =============================================================================
-- Token Helper
-- =============================================================================

withToken :: (OAuth2Token -> IO a) -> IO a
withToken action = do
  oauth2 <- loadClientSecrets
  token <- getOrRefreshToken oauth2
  action token

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

  RemoveVideo itemId -> withToken $ \token -> do
    success <- removeVideo token itemId
    putStrLn $ if success then "Removed: " ++ T.unpack itemId else "Failed to remove video"

  CreatePlaylist Nothing _ _ -> interactiveCreatePlaylist
  CreatePlaylist (Just title) Nothing _ -> withToken $ \token -> do
    mbId <- createPlaylist token title "" "private"
    case mbId of
      Just pid -> putStrLn $ T.unpack pid
      Nothing -> putStrLn "Failed to create playlist"
  CreatePlaylist (Just title) (Just desc) Nothing -> withToken $ \token -> do
    mbId <- createPlaylist token title desc "private"
    case mbId of
      Just pid -> putStrLn $ T.unpack pid
      Nothing -> putStrLn "Failed to create playlist"
  CreatePlaylist (Just title) (Just desc) (Just privacy) -> withToken $ \token -> do
    mbId <- createPlaylist token title desc privacy
    case mbId of
      Just pid -> putStrLn $ T.unpack pid
      Nothing -> putStrLn "Failed to create playlist"

  AddVideo pid vid -> withToken $ \token -> do
    success <- addVideo token pid vid
    putStrLn $ if success
      then "Added video " ++ T.unpack vid ++ " to playlist"
      else "Failed to add video"

  DeletePlaylist pid -> withToken $ \token -> do
    success <- deletePlaylist token pid
    putStrLn $ if success then "Deleted playlist: " ++ T.unpack pid else "Failed to delete playlist"

  AddVideos pid file -> do
    videoIds <- readVideoIds file
    putStrLn $ "Adding " ++ show (length videoIds) ++ " videos to playlist..."
    withToken $ \token -> do
      (successIds, failedIds) <- addVideos token pid videoIds
      putStrLn $ "\nDone! Added: " ++ show (length successIds) ++ ", Failed: " ++ show (length failedIds)

  RemoveVideos pid file -> do
    videoIds <- readVideoIds file
    putStrLn $ "Removing " ++ show (length videoIds) ++ " videos from playlist..."
    withToken $ \token -> do
      (success, failed) <- removeVideosByVideoId token pid videoIds
      putStrLn $ "\nDone! Removed: " ++ show success ++ ", Failed: " ++ show failed

  MoveVideos src dst file -> do
    videoIds <- readVideoIds file
    putStrLn $ "Moving " ++ show (length videoIds) ++ " videos..."
    withToken $ \token -> do
      (added, removed, failed) <- moveVideos token src dst videoIds
      putStrLn $ "\nDone! Added: " ++ show added ++ ", Removed: " ++ show removed ++ ", Failed: " ++ show failed

  Interactive -> interactiveMode

readVideoIds :: FilePath -> IO [T.Text]
readVideoIds file = do
  content <- readFile file
  return $ map T.pack $ filter (not . null) $ lines content

-- =============================================================================
-- Interactive Mode
-- =============================================================================

interactiveCreatePlaylist :: IO ()
interactiveCreatePlaylist = do
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
  withToken $ \token -> do
    mbId <- createPlaylist token title desc (T.pack privacyStatus)
    case mbId of
      Just pid -> putStrLn $ "Created playlist: " ++ T.unpack pid
      Nothing -> putStrLn "Failed to create playlist"

safeCmd :: IO () -> IO ()
safeCmd action = do
  result <- try @SomeException action
  case result of
    Left e -> putStrLn $ "Error: " ++ show e
    Right () -> return ()

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
        "p" -> safeCmd cmdListPlaylists >> loop
        "l" -> cmdListVideos >> loop
        "r" -> cmdRemoveVideo >> loop
        "c" -> cmdCreatePlaylist >> loop
        "a" -> cmdAddVideo >> loop
        "d" -> cmdDeletePlaylist >> loop
        "q" -> putStrLn "Goodbye!"
        _ -> putStrLn "Unknown command" >> loop

cmdListPlaylists :: IO ()
cmdListPlaylists = withToken listPlaylists

cmdListVideos :: IO ()
cmdListVideos = do
  putStr "Enter playlist ID: "
  hFlush stdout
  pid <- T.getLine
  safeCmd $ withToken (`listPlaylistVideos` pid)

cmdRemoveVideo :: IO ()
cmdRemoveVideo = do
  putStr "Enter playlist item ID to remove: "
  hFlush stdout
  itemId <- T.getLine
  safeCmd $ withToken $ \token -> do
    success <- removeVideo token itemId
    putStrLn $ if success then "Removed successfully" else "Failed to remove"

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
  safeCmd $ withToken $ \token -> do
    mbId <- createPlaylist token title desc privacy
    case mbId of
      Just pid -> putStrLn $ "Created: " ++ T.unpack pid
      Nothing -> putStrLn "Failed to create playlist"

cmdAddVideo :: IO ()
cmdAddVideo = do
  putStr "Playlist ID: "
  hFlush stdout
  pid <- T.getLine
  putStr "Video ID: "
  hFlush stdout
  vid <- T.getLine
  safeCmd $ withToken $ \token -> do
    success <- addVideo token pid vid
    putStrLn $ if success then "Added successfully" else "Failed to add"

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
    else safeCmd $ withToken $ \token -> do
      success <- deletePlaylist token pid
      putStrLn $ if success then "Deleted successfully" else "Failed to delete"
