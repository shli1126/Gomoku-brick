{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use lambda-case" #-}
module UI 
    ( drawUI
    , emptyGrid
    , theMap  -- Make sure this is exported
    , runUI
    ) where
        
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Border.Style (BorderStyle, unicodeBold)
import Brick.Widgets.Core (hLimit, vLimit)
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Util (on)
import Brick (Next)
import Graphics.Vty.Attributes (defAttr, bold, withStyle)
import Type (Player, Board, Game(..))
import Type( Direction(..) )
import Type as T
import Logic
import qualified Graphics.Vty as V


-- Define custom attribute for larger caption
captionAttr :: AttrName
captionAttr = attrName "caption"

-- Define the cell and grid types
data Cell = Empty | X | O



theMap :: AttrMap
theMap = attrMap defAttr
    [ (captionAttr, withStyle defAttr bold)
    ]


-- Function to print a single Cell
printCell :: Cell -> String
printCell Empty = "     "
printCell X     = "  X  "
printCell O     = "  O  "

-- Function to create a widget for a single Cell
drawCell :: Cell -> Widget ()
drawCell cell = 
    withBorderStyle borderStyle
    $ border
    $ hLimit 4 -- Reduced horizontal limit
    $ vLimit 2 -- Reduced vertical limit
    $ padAll 0
    $ str
    $ printCell cell

-- Function to create a single row Widget
drawRow :: [Cell] -> Widget ()
drawRow cells = hBox $ map drawCell cells

-- Function to create the grid Widget
drawGrid :: [[Cell]] -> Widget ()
drawGrid grid =
    withBorderStyle borderStyle
        . border
        . vBox $ map drawRow grid


-- Example grid (10x10 empty cells)
emptyGrid :: [[Cell]]
emptyGrid = replicate 10 (replicate 10 Empty)

-- try to fit the board data type into the UI
uiRepresentation :: Board -> [[Cell]]
uiRepresentation board =
    map (map (\cell -> case cell of
                               -1 -> Empty
                               1 -> X
                               _ -> O)) board


-- Styling for the borders (you can choose a different style)
borderStyle :: BorderStyle
borderStyle = unicodeBold



currentPlayerString :: Game -> String
currentPlayerString game = "Current Player: " ++ name player
  where
    player = players game !! nextPlayer game

-- boomsLeftString :: String -> Int -> String
-- boomsLeftString playerName count = "Boom left for " ++ playerName ++ ": " ++ show count

boomsLeftStringPlayer1 :: Game -> String
boomsLeftStringPlayer1 game = "Boom left for " ++ (name player) ++ ": " ++ show (boomsLeft player)
  where
    player = players game !! 0

boomsLeftStringPlayer2 :: Game -> String
boomsLeftStringPlayer2 game = "Boom left for " ++ (name player) ++ ": " ++ show (boomsLeft player)
  where
    player = players game !! 1

-- paddedBox :: Int -> Widget n -> Widget n
-- paddedBox  content = 
--     vBox [ 
--         padTop content 
--     ]


-- Function to draw the entire UI
-- drawUI :: Board -> Widget ()
-- drawUI board =
--     center $
--         vBox [
--             withAttr captionAttr $ str "Five in a ROW",
--             drawGrid (uiRepresentation board),
--             vBox [  -- Change here to vBox for vertical alignment
--                  1 $ str (currentPlayerString "Dummy Player 1"),
--                  1 $ str (boomsLeftString "Dummy Player 1" 3),
--                  1 $ str (boomsLeftString "Dummy Player 2" 2)
--             ]
--         ]

drawUI :: Game -> Widget ()
drawUI game = 
    let board = Type.board game -- Extract the board from the Game
    in center $
         vBox [
            withAttr captionAttr $ str "Five in a ROW",
            drawGrid (uiRepresentation board),
            vBox [  -- Change here to vBox for vertical alignment
                str (currentPlayerString game),
                str (boomsLeftStringPlayer1 game),
                str (boomsLeftStringPlayer2 game)
            ]
        ]



testBoard :: [[Int]]
testBoard = [[-1, -1,-1,-1,-1,-1],
                  [-1,-1, -1,-1,-1,-1],
                  [-1,-1,-1, -1,-1,-1],
                  [-1,-1,-1,-1, -1,-1],
                  [-1,-1,-1,-1,-1, -1],
                  [-1,-1,-1,-1,-1,-1]]



runUI :: IO Game
runUI = defaultMain app (init1 10 ["Player1", "Player 2"])


-- app :: App Game e ()
-- app = App
--   { appDraw         = \x -> (drawUI x)
--   , appChooseCursor = neverShowCursor
--   , appHandleEvent  = handleEvent
--   , appStartEvent   = return ()
--   , appAttrMap      = const theMap
--   }

-- Define showCursor function
-- displayCursor game _ =
--     case cursor game of
--         (col, row) -> Just $ CursorLocation (Location col row)
--         _ -> Nothing



app :: App Game e ()
app = App
  { appDraw         = \game -> [drawUI game]   -- Use the modified drawUI function
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const theMap
  }




handleEvent :: Game -> BrickEvent () e -> EventM () (Next Game)
handleEvent game (VtyEvent (V.EvKey key [])) = case key of
  V.KUp  -> continue (moveCursor game T.Up)
  V.KDown -> continue (moveCursor game T.Down)
  V.KLeft -> continue (moveCursor game T.Left)
  V.KRight -> continue (moveCursor game T.Right)
  V.KChar 'u' -> continue (undo game)
  V.KChar 'b' -> continue (fst $ boom game)
  V.KEnter -> continue (fst $ dodo game)
  _ -> continue game
handleEvent game _ = continue game


