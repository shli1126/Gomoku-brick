module Type (module Type) where

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

data Direction = Up | Down | Left | Right deriving (Show)
