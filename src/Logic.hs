module Logic (init, reset, moveCursor, undo) where

import Type (Game, Board)

init :: Int -> [String] -> Game
init size names = Game
  { cursor = (size `div` 2, size `div` 2)
  , board = B $ replicate size (replicate size 0)
  , players = map (\name -> Player name 1) names
  , prev = Nothing
  }

reset :: Game -> Game
reset game = game
  { board = B $ replicate size (replicate size 0)
  , players = map (\player -> player { boomsLeft = 1 }) (player game)
  , prev = Nothing
  }
    where
      size = length (board game)

moveCursor :: Game -> Direction -> Game
moveCursor game dir = game { cursor = newCursor }
  where
    (x, y) = cursor game
    newCursor = case dir of
      Up    -> (x, (y - 1 + size) `mod` size)
      Down  -> (x, (y + 1) `mod` size)
      Left  -> ((x - 1 + size) `mod` size, y)
      Right -> ((x + 1) `mod` size, y)

undo :: Game -> Game
undo game = case prev game of
  Nothing -> game
  Just game' -> game'
