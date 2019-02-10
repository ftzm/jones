module Jones
    ( runJones
    ) where

import Control.Monad
import Brick
import Types
import Git
import Draw
import Events

app :: App AppState () Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const myAttrMap
  }

runJones :: IO ()
runJones = getGit >>= void . defaultMain app . startAppState
