{-# LANGUAGE OverloadedStrings #-}

module Draw
    ( drawUI
    , myAttrMap
    ) where

import Data.Maybe
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Data.Vector as VC
import Control.Lens
import Control.Zipper
import Control.Zipper.Internal

import Types

-------------------------------------------------------------------------------
-- Attr

toggledAttr = "toggledAttr" :: AttrName
listingAttr = "listingAttr" :: AttrName

myAttrMap = attrMap V.defAttr
  [ (toggledAttr, V.black `on` V.white)
  , (listingAttr, V.blue `on` V.black)
  ]

-------------------------------------------------------------------------------

drawEntry :: ListingEntry -> Bool -> Widget Name
drawEntry e b = str (artifactType e)
            <=> str (entryHash e)
            <=> str (artifactId e)
            <=> str (show b)

drawEntries :: ListingEntries -> Widget Name
drawEntries ls = fromMaybe (str "No Artifacts") $ do
    zipper <- ls
    let list = rezip zipper
    let index = focalPoint zipper
    return $ VC.ifoldl' (f index) emptyWidget list
  where f :: Int -> Widget Name -> Int -> ListingEntry -> Widget Name
        f foc acc i x = acc <=> drawEntry x (i == foc)


drawListing :: Maybe Listing -> Widget Name
drawListing Nothing = str "No listing"
drawListing (Just l) =
  vBox
  [ str $ "Build Date:       " ++ l ^. buildDate
  , str $ "Build User:       " ++ l ^. buildUser
  , str $ "Previous Listing: " ++ l ^. previousListing
  , str "Entries:"
  , drawEntries $ l ^. artifacts
  ]

renderListingList :: Bool -> CommitInfo -> Widget Name
renderListingList b c
  | b = withAttr toggledAttr $ str $ _hash c
  | isJust $ _listing c = withAttr listingAttr $ str $ _hash c
  | otherwise = str $ _hash c

drawUI :: AppState -> [Widget Name]
drawUI s = [left <+> right]
  where left = B.borderWithLabel (str "Commits") $ hLimit 40 $ L.renderList renderListingList True (_commits s)
        right = B.borderWithLabel (str "content") (drawListing currentListing <=> fill ' ')
        currentListing :: Maybe Listing
        currentListing = do
          (int, commitInfo) <- L.listSelectedElement $ _commits s
          _listing commitInfo
