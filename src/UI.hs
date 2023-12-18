{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use lambda-case" #-}
module UI
    ( drawUI
    , theMap 
    , runUI
    ) where

import Brick as B
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Border.Style
import Graphics.Vty.Attributes (defAttr, bold, withStyle)
import Type (Player, Board, Game(..), Direction(..) )
import Type as T
import Logic
import qualified Graphics.Vty as V


-- Define custom attribute for larger caption
captionAttr :: AttrName
captionAttr = attrName "caption"

-- Define the cell and grid types
data Cell = Empty | X | O deriving (Show, Eq)
data CellCursor = CC {cell :: Cell, strong :: Bool} deriving (Show, Eq)


-- Define a new attribute for bold text
boldTextAttr :: AttrName
boldTextAttr = attrName "boldText"


-- creates a new attribute map with a default attribute and a list of specific attribute mappings
theMap :: AttrMap
theMap = attrMap defAttr
    [ (captionAttr, withStyle defAttr bold)
    , (boldTextAttr, withStyle defAttr bold)
    ]


-- First draw the cell
drawCell :: CellCursor -> Widget ()
drawCell cellcursor =
    let content = case cell cellcursor of
                    Empty -> str "   "
                    X -> withAttr boldTextAttr (str " X ")
                    O -> withAttr boldTextAttr (str " O ")
    in withBorderStyle (borderStyle (strong cellcursor))
       $ border
       $ hLimit 3
       $ vLimit 2
       $ content


-- Function to create a single row Widget
-- applies the drawCell function to each CellCursor in cells
-- hBox takes a list of Widgets and arranges them horizontally side by side
drawRow :: [CellCursor] -> Widget ()
drawRow cells = hBox $ map drawCell cells

-- Function to create the grid Widget
drawGrid :: [[CellCursor]] -> Widget ()
drawGrid grid = withBorderStyle (borderStyle True) . border . vBox $ map drawRow grid


-- fit the board data type into the UI
uiRepresentation :: Board -> [[Cell]]
uiRepresentation board =
    map (map (\cell -> case cell of
                               -1 -> Empty
                               1 -> X
                               _ -> O)) board


borderStyle :: Bool -> BorderStyle
borderStyle isStrong = if isStrong then unicodeBold else unicode


boomsLeftStringPlayer :: Game -> Int -> String
boomsLeftStringPlayer game n = name player ++ " has " ++ show (boomsLeft player) ++ if boomsLeft player == 1 then " boom left" else " booms left"
  where
    player = players game !! n


displayStatus :: Game -> String
displayStatus game
  | isWin (board game) = name player ++ [" (O)", " (X)"] !! nextPlayer game ++ " wins!!!"
  | isTie (board game) = "Tie!!!"
  | otherwise = "Current player: " ++ name player ++ [" (O)", " (X)"] !! nextPlayer game
  where
    player = players game !! nextPlayer game


displayScorePlayer :: Game -> Int -> String
displayScorePlayer game n = "Total score of " ++ name player ++ ": " ++ show (score player)
  where
    player = players game !! n

displayRules :: Game -> String
displayRules game = if isWin (board game) || isTie (board game)
  then "Press R to start a new game                \nPress Ctrl-Z to quit               \n                                     \n                     \n               \n                  \n"
  else "Place stones to form an unbroken row to win\nPress arrow keys to move the cursor\nPress enter or space to place a stone\nPress B to use a boom\nPress U to undo\nPress Ctrl-Z to quit"

drawUI :: Game -> Widget ()
drawUI game =
    let board = Type.board game
    in center $
         vBox [
            withAttr captionAttr $ str "Five in a ROW",
            let cells = uiRepresentation board
                cursorX = fst (cursor game)
                cursorY = snd (cursor game)
                cellCursors = [[CC cell (x == cursorX && y == cursorY) | (x, cell) <- zip [0..] row] | (y, row) <- zip [0..] cells]
            in
                drawGrid cellCursors,
            vBox [
                str (displayStatus game),
                str " ",
                str (boomsLeftStringPlayer game 0),
                str (boomsLeftStringPlayer game 1),
                str " ",
                str (displayScorePlayer game 0),
                str (displayScorePlayer game 1),
                str " ",
                str (displayRules game)
            ]
        ]


runUI :: Int -> [String] -> IO Game
runUI size playerNames = defaultMain app (init1 size playerNames)

app :: App Game e ()
app = App
  { appDraw         = \game -> [drawUI game]
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const theMap
  }


handleEvent :: Game -> BrickEvent () e -> EventM () (Next Game)
handleEvent game (VtyEvent (V.EvKey (V.KChar 'z') [V.MCtrl])) = halt game
handleEvent game (VtyEvent (V.EvKey key [])) =
  if isTie (board game) || isWin (board game) then (case key of
            V.KChar 'r' -> continue (reset game)
            _ -> continue game) else (case key of
    V.KUp  -> continue (moveCursor game T.Up)
    V.KDown -> continue (moveCursor game T.Down)
    V.KLeft -> continue (moveCursor game T.Left)
    V.KRight -> continue (moveCursor game T.Right)
    V.KChar 'u' -> continue (undo game)
    V.KChar 'b' -> continue (fst $ boom game)
    V.KChar ' ' -> continue (fst $ dodo game)
    V.KEnter -> continue (fst $ dodo game)
    _ -> continue game)
handleEvent game _ = continue game





