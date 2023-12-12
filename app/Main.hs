module Main (module Main) where

import Type
import UI
import Logic
import Brick


app :: App Game e n
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor -- Or your own cursor function
  , appHandleEvent = handleEvent -- Define how to handle events
  , appStartEvent = return -- Initialization, if needed
  , appAttrMap = const theMap -- Define your attribute map, if needed
  }


main :: IO ()
main = do
  let size = 6
  let names = ["Player 1", "Player 2"]
  let initialGame = Logic.init1 size names
  let widgets = drawUI (board initialGame)
  appStart (Just "Five in a Row") widgets

