{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use lambda-case" #-}
module UI 
    ( drawUI
    , emptyGrid
    , theMap  -- Make sure this is exported
    , runUI
    ) where
        
import Brick as B
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Border.Style (BorderStyle, unicodeBold)
import Graphics.Vty.Attributes (defAttr, bold, withStyle)
import Type (Player, Board, Game(..), Direction(..) )
import Type as T
import Logic
import qualified Graphics.Vty as V


-- Define custom attribute for larger caption
captionAttr :: AttrName
captionAttr = attrName "caption"



-- Define the cell and grid types
data Cell = Empty | X | O


-- Define a new attribute for bold text
boldTextAttr :: AttrName
boldTextAttr = attrName "boldText"


theMap :: AttrMap
theMap = attrMap defAttr
    [ (captionAttr, withStyle defAttr bold)
    , (boldTextAttr, withStyle defAttr bold) -- This makes the text bold
    ]


-- Function to print a single Cell
printCell :: Cell -> String
printCell Empty = "     "
printCell X     = " X  "
printCell O     = " O  "


drawCell :: Cell -> Widget ()
drawCell cell =
    let content = case cell of
                    Empty -> str "     "
                    X     -> withAttr boldTextAttr $ str " X "
                    O     -> withAttr boldTextAttr $ str " O "
    in withBorderStyle borderStyle
       $ border
       $ hLimit 3
       $ vLimit 2
       $ padAll 0
       $ content  -- Use the content here


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



currentPlayerString :: String -> String
currentPlayerString name = "Current Player: " ++ name

boomsLeftString :: String -> Int -> String
boomsLeftString playerName count = "Boom left for " ++ playerName ++ ": " ++ show count


drawUI :: Game -> Widget ()
drawUI game = 
    let board = Type.board game -- Extract the board from the Game
    in center $
         vBox [
            withAttr captionAttr $ str "Five in a ROW",
            drawGrid (uiRepresentation board),
            vBox [  -- Change here to vBox for vertical alignment
                str (currentPlayerString "Dummy Player 1"),
                str (boomsLeftString "Dummy Player 1" 3),
                str (boomsLeftString "Dummy Player 2" 2)
            ]
        ]



runUI :: IO Game
runUI = defaultMain app (init1 10 ["Player1", "Player 2"])


cursorPos :: Game -> Location
cursorPos game = let (x, y) = cursor game in Location (leftOffset + screenX x, topOffset + screenY y)

-- These offsets should represent the starting point of your grid on the screen.
topOffset :: Int
topOffset = 18 -- Number of lines before the board starts, adjust accordingly

leftOffset :: Int
leftOffset = 84 -- Number of characters from the left of the screen, adjust accordingly

-- Assume each cell is 5 characters wide including the borders and padding
cellWidth :: Int
cellWidth = 5 -- Change this according to your actual cell width

-- Assume each cell is 3 characters tall including the borders and padding
cellHeight :: Int
cellHeight = 3 -- Change this according to your actual cell height

-- Converts game coordinates to screen coordinates
screenX :: Int -> Int
screenX x = x * cellWidth 

screenY :: Int -> Int
screenY y = y * cellHeight


displayCursor :: Game -> [CursorLocation n] -> Maybe (CursorLocation n)
displayCursor game cursors = Just $ CursorLocation
    { cursorLocation = cursorPos game
    , cursorLocationName = Nothing
    , cursorLocationVisible = True
    }




app :: App Game e ()
app = App
  { appDraw         = \game -> [drawUI game]   -- Use the modified drawUI function
  , appChooseCursor = displayCursor
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




