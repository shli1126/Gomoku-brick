module Type (Game(..), Board(..), Player(..)) where


-- | Represents the game board for Five in a Row.
-- | It is a newtype that wraps a nested list of integers,
-- | where each integer represents a cell on the board.
-- | 0 represents an empty cell, 1 represents X, and 2 represents O.
newtype Board = Board [[Int]]


data Game = Game
  { cursor :: (Int, Int)
  , board :: Board
  , players :: [Player]
  , prev :: Maybe Game
  }

data Player = Player
  { name :: String
  , boomsLeft :: Int
  }

