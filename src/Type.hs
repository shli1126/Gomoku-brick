module Type (module Type) where

-- Board[i][j] == -1 : Empty slot
-- Board[i][j] >= 0  : Player index
-- Maybe require a newtype for Board Slot, and maybe not
type Board = [[Int]]

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

data Direction = Up | Down | Left | Right deriving (Show)
