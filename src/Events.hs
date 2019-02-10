module Events
    ( handleEvent
    ) where

import Brick
import Control.Zipper
import Control.Zipper.Internal
import Control.Lens
import qualified Graphics.Vty as V
import qualified Brick.Widgets.List as L

import Types

travEntries :: Traversal' CommitInfo ListingEntries'
travEntries = listing . _Just . artifacts . _Just

handleEvent :: AppState -> BrickEvent n e -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar k) [])) = continue $ action s
  where
    action = case k of
      'k' -> commits %~ L.listMoveUp
      'j' -> commits %~ L.listMoveDown
      'J' -> commits %~ L.listModify (travEntries %~ tug rightward)
      'K' -> commits %~ L.listModify (travEntries %~ tug leftward)
      _   -> id
handleEvent s _ = continue s
