{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Types
  ( -- OAuth types
    ClientSecrets(..)
  , ClientConfig(..)
  , TokenResponse(..)
  , StoredToken(..)
    -- Playlist types
  , PlaylistsResponse(..)
  , PlaylistInfo(..)
  , PlaylistSnippetInfo(..)
  , PlaylistContentDetails(..)
    -- Playlist item types
  , PlaylistItemsResponse(..)
  , PlaylistItem(..)
  , PlaylistItemSnippet(..)
  , ResourceId(..)
    -- Create playlist types
  , CreatePlaylistRequest(..)
  , CreatePlaylistSnippet(..)
  , CreatePlaylistStatus(..)
  , CreatePlaylistResponse(..)
    -- Add video types
  , AddVideoRequest(..)
  , AddVideoSnippet(..)
  , AddVideoResourceId(..)
  ) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

-- =============================================================================
-- OAuth Types
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

-- =============================================================================
-- Playlist Types
-- =============================================================================

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

-- =============================================================================
-- Playlist Item Types
-- =============================================================================

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
  , plitem_position :: Int
  , plitem_resourceId :: ResourceId
  } deriving stock (Show, Generic)

instance FromJSON PlaylistItemSnippet where
  parseJSON = withObject "PlaylistItemSnippet" $ \v -> PlaylistItemSnippet
    <$> v .: "title"
    <*> v .: "position"
    <*> v .: "resourceId"

data ResourceId = ResourceId
  { res_videoId :: T.Text
  } deriving stock (Show, Generic)

instance FromJSON ResourceId where
  parseJSON = withObject "ResourceId" $ \v ->
    ResourceId <$> v .: "videoId"

-- =============================================================================
-- Create Playlist Types
-- =============================================================================

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

-- =============================================================================
-- Add Video Types
-- =============================================================================

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
