{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Data.Text
import Data.HashMap.Strict
import Data.Yaml
import Data.Vector as V
import Control.Lens
import Control.Lens.TH
import Control.Zipper
import Control.Type.Operator
import qualified Brick.Widgets.List as L

data ListingEntry = ListingEntry
  { moduleName :: String
  , artifactType :: String
  , entryHash :: String
  , artifactId :: String
  } deriving (Show)

type ListingEntries' = Top :>> Vector ListingEntry :>> ListingEntry
type ListingEntries = Maybe ListingEntries'

parseListingEntry :: Text -> Value -> Parser ListingEntry
parseListingEntry k = withObject "ListingEntry" $ \v -> ListingEntry
  <$> return (show k)
  <*> v .: "artifact-type"
  <*> v .: "git-commit-hash"
  <*> v .: "artifact-id"

parseListingEntries :: Value -> Parser ListingEntries
parseListingEntries
  = withObject "Artifacts"          -- assume Value is Object
  $ fmap (within traverse . zipper) -- create zipper and enter contents
  . foldlWithKey' f (pure V.empty)  -- parse hashmap into Vector ListingEntry
  where
    f a k v = V.snoc <$> a <*> parseListingEntry k v

data Listing = Listing
  { _buildDate :: String
  , _buildUser :: String
  , _previousListing :: String
  , _artifacts :: ListingEntries
  }
makeLenses ''Listing

instance FromJSON Listing where
    parseJSON = withObject "Listing" $ \v -> Listing
        <$> v .: "build-date"
        <*> v .: "build-user"
        <*> v .: "previous-listing"
        <*> (parseListingEntries =<< (v .: "artifacts"))

data CommitInfo = CommitInfo
  { _hash :: String
  , _author :: String
  , _subject :: String
  , _body :: String
  , _listing :: Maybe Listing
  }
makeLenses ''CommitInfo

data Name = ViewPort1 | ViewPort2
  deriving (Eq, Ord, Show)

data AppState = AppState
  { _commits :: L.List Name CommitInfo
  , _inspect  :: Bool
  }
makeLenses ''AppState

startAppState :: V.Vector CommitInfo -> AppState
startAppState v = AppState (L.list ViewPort1 v 1) False
