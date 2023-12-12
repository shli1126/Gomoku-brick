module Type (module Type) where

-- Board[i][j] == -1 : Empty slot
-- Board[i][j] >= 0  : Player index
-- Maybe require a newtype for Board Slot, and maybe not
type Board = [[Int]]

data Game = Game
  { cursor :: (Int, Int) -- (x, y) s.t. board !! y !! x is pos
  , board :: Board
  , players :: [Player]
  , prev :: Maybe Game
  , nextPlayer :: Int -- player to take action
  }

data Player = Player
  { name :: String
  , boomsLeft :: Int
  , score :: Int
  }

data Direction = Up | Down | Left | Right deriving (Show)
data DoStatus = Valid | Invalid | Win deriving (Show)