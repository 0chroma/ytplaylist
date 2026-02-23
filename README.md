# YouTube Playlist Manager

A Haskell command-line tool to manage YouTube playlists using the YouTube Data API v3.

## Features

- **List** all your YouTube playlists
- **List** all videos in any playlist (handles 800+ videos with pagination)
- **Create** new playlists (public, unlisted, or private)
- **Add** videos to playlists by video ID
- **Remove** videos by video ID
- **Move** videos between playlists
- **Delete** entire playlists
- **Batch operations** for adding, removing, or moving multiple videos from a file

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/)
- OAuth2 credentials from Google Cloud Console

## Setup

### 1. Get OAuth2 Credentials

1. Go to [Google Cloud Console](https://console.cloud.google.com/)
2. Create a new project (or use existing)
3. Enable the **YouTube Data API v3**:
   - APIs & Services > Library > Search "YouTube Data API v3" > Enable
4. Create OAuth2 credentials:
   - APIs & Services > Credentials > Create Credentials > OAuth client ID
   - Application type: "Desktop app"
   - Download the JSON and save as `client_secrets.json` in this directory

### 2. Build

```bash
cabal build
```

## Usage

### First-time Authentication

```bash
cabal run ytplaylist -- auth
# or after building:
./ytplaylist auth
```

This will open your browser for Google authentication and save the token to `ytplaylist_token.json` in the current directory.

### List Your Playlists

```bash
./ytplaylist list-playlists
```

Shows all your playlists with their IDs and video counts.

### List Videos in a Playlist

```bash
./ytplaylist list <playlist-id>
```

Shows all videos (Item ID, Video ID, Title). Works with playlists of any size using pagination.

### Create a New Playlist

```bash
./ytplaylist create-playlist <title> <description> <privacy>
```

Privacy can be: `public`, `unlisted`, or `private`.

Example:
```bash
./ytplaylist create-playlist "My Music" "Favorite songs" "private"
```

### Add a Video to a Playlist

```bash
./ytplaylist add <playlist-id> <video-id>
```

Video ID is the part after `v=` in YouTube URLs:
- URL: `https://www.youtube.com/watch?v=dQw4w9WgXcQ`
- Video ID: `dQw4w9WgXcQ`

### Add Multiple Videos (Batch)

```bash
./ytplaylist add-batch <playlist-id> <file>
```

The file should contain one video ID per line.

### Remove a Video from a Playlist

```bash
./ytplaylist remove <playlist-id> <video-id>
```

### Remove Multiple Videos (Batch)

```bash
./ytplaylist remove-batch <playlist-id> <file>
```

The file should contain one video ID per line.

### Move a Video Between Playlists

```bash
./ytplaylist move <source-playlist-id> <target-playlist-id> <video-id>
```

### Move Multiple Videos (Batch)

```bash
./ytplaylist move-batch <source-playlist-id> <target-playlist-id> <file>
```

The file should contain one video ID per line.

### Delete a Playlist

```bash
./ytplaylist delete-playlist <playlist-id>
```

## Command Reference

| Command | Description |
|---------|-------------|
| `auth` | Authenticate with YouTube |
| `list-playlists` | List all your playlists |
| `list <playlist-id>` | List videos in a playlist |
| `create-playlist <title> <description> <privacy>` | Create a new playlist |
| `add <playlist-id> <video-id>` | Add a single video to playlist |
| `add-batch <playlist-id> <file>` | Add multiple videos from file |
| `remove <playlist-id> <video-id>` | Remove a single video from playlist |
| `remove-batch <playlist-id> <file>` | Remove multiple videos from file |
| `move <source> <target> <video-id>` | Move a single video between playlists |
| `move-batch <source> <target> <file>` | Move multiple videos from file |
| `delete-playlist <playlist-id>` | Delete a playlist |

## Examples

```bash
# Authenticate
./ytplaylist auth

# See your playlists
./ytplaylist list-playlists
# Output: My Playlist | ID: PLxxxxxxxxxxx | Videos: 42

# List videos
./ytplaylist list PLxxxxxxxxxxx

# Create a new playlist
./ytplaylist create-playlist "My Music" "Favorite songs" "private"

# Add a video
./ytplaylist add PLxxxxxxxxxxx dQw4w9WgXcQ

# Add multiple videos from file
./ytplaylist add-batch PLxxxxxxxxxxx videos.txt

# Remove a video
./ytplaylist remove PLxxxxxxxxxxx dQw4w9WgXcQ

# Move a video between playlists
./ytplaylist move PLxxxxxxxxxxx PLyyyyyyyyyyy dQw4w9WgXcQ

# Delete a playlist
./ytplaylist delete-playlist PLxxxxxxxxxxx
```

## File Structure

```
.
├── client_secrets.json       # Your OAuth2 credentials (DO NOT COMMIT)
├── ytplaylist_token.json     # Authentication token (DO NOT COMMIT)
├── .gitignore               # Ignores credentials and build artifacts
├── youtube-playlist-manager.cabal
├── Main.hs                  # Source code
├── src/                     # Library modules
│   ├── HTTP.hs
│   ├── OAuth.hs
│   ├── Types.hs
│   └── YouTube.hs
├── test/Test.hs             # Test suite
└── README.md
```

## Security Notes

- `client_secrets.json` and `ytplaylist_token.json` contain sensitive credentials
- These are listed in `.gitignore` and should NEVER be committed
- Each project directory has its own separate authentication token

## Testing

```bash
# Run all tests (requires authentication)
cabal test

# The integration test creates and deletes a real playlist
```

## Troubleshooting

**"Authentication token not found: ytplaylist_token.json"**
Run `./ytplaylist auth` first

**"Failed to parse client_secrets.json"**
Make sure you downloaded the Desktop app credentials from Google Cloud Console

**Browser doesn't open**
Copy the URL from the console and paste it manually

**"Quota exceeded"**
YouTube API has daily quotas (default 10,000 units/day). Creating playlists and adding videos each cost ~50 units.

## API Quotas

The YouTube Data API has quota limits (default 10,000 units/day):
- Create playlist: ~50 units
- Add video: ~50 units
- Delete item: ~50 units
- List operations: ~1-5 units

## License

MIT
