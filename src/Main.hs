module Main(main) where

import Brick
import Brick.Main (App(..), defaultMain, neverShowCursor)
import Graphics.Vty (Key(..)) -- Import for EvKey and KChar
import Graphics.Vty( Event( EvKey ) )
import UI (drawUI, theMap)
import Type (Game(..), Board(..), Player(..))


-- Define an initial state for your application
initialState :: Game
initialState = Game
  { cursor = (0, 0)
  , board = Board $ replicate 10 (replicate 10 0)
  , players = [Player "Player1" 3, Player "Player2" 3]
  , prev = Nothing
  }

-- Define your event handler
handleEvent :: BrickEvent n e -> EventM n Game ()
handleEvent (VtyEvent (EvKey (KChar 'q') [])) = halt  -- Quit on 'q' press
handleEvent _ = return ()  -- Ignore all other events

app :: App Game e ()
app = App {
  appDraw = drawUI . board,
  appChooseCursor = neverShowCursor,
  appHandleEvent = handleEvent,
  appStartEvent = return (),
  appAttrMap = const theMap
}


main :: IO ()
main = defaultMain app initialState

















