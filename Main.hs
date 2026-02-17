{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout, stderr, hSetBuffering, BufferMode(..))
import Control.Exception (try, SomeException)

import Types
import OAuth
import YouTube

-- =============================================================================
-- Main Program
-- =============================================================================

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
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

    ["create-playlist", title] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      mbId <- createPlaylist token (T.pack title) "" "private"
      case mbId of
        Just pid -> putStrLn $ T.unpack pid
        Nothing -> putStrLn "Failed to create playlist"
      exitSuccess

    ["create-playlist", title, privacy] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      mbId <- createPlaylist token (T.pack title) "" (T.pack privacy)
      case mbId of
        Just pid -> putStrLn $ T.unpack pid
        Nothing -> putStrLn "Failed to create playlist"
      exitSuccess

    ["create-playlist", title, description, privacy] -> do
      secrets <- loadClientSecrets
      token <- getOrRefreshToken (installed secrets)
      mbId <- createPlaylist token (T.pack title) (T.pack description) (T.pack privacy)
      case mbId of
        Just pid -> putStrLn $ T.unpack pid
        Nothing -> putStrLn "Failed to create playlist"
      exitSuccess

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
  putStrLn "  create-playlist <title>           Create playlist (private, no description)"
  putStrLn "  create-playlist <title> <privacy> Create playlist with privacy setting"
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
