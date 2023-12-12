{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use lambda-case" #-}
module UI 
    ( drawUI
    , emptyGrid
    , theMap  -- Make sure this is exported
    ) where
        
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Border.Style (BorderStyle, unicodeBold)
import Brick.Widgets.Core (hLimit, vLimit)
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Util (on)
import Graphics.Vty.Attributes (defAttr, bold, withStyle)
import Type 

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
                               0 -> Empty
                               1 -> X
                               _ -> O)) board

-- Function to draw the entire UI
-- drawUI :: Grid -> [Widget ()]
-- drawUI grid = [center $ drawGrid grid]
drawUI :: Board -> [Widget ()]
drawUI board =
    [center $ 
        (withAttr captionAttr $ str "Five in a ROW") <=>
        drawGrid (uiRepresentation board)
    ]


-- Styling for the borders (you can choose a different style)
borderStyle :: BorderStyle
borderStyle = unicodeBold