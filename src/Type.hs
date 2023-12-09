module Type (Game, Board) where

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
