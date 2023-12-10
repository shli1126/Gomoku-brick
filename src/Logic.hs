{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module Logic (module Logic) where

import Type as T

init1 :: Int -> [String] -> Game
init1 size names = Game
  { cursor = (size `div` 2, size `div` 2)
  , board = replicate size (replicate size (-1))
  , players = map (\name -> Player name 2) names
  , prev = Nothing
  }

-- TODO: fix it
-- reset :: Game -> Game
-- reset game = game
--   { board =  replicate size (replicate size 0)
--   , players = map (\player -> player { boomsLeft = 1 }) (player game)
--   , prev = Nothing
--   }
--     where
--       size = length (board game)

moveCursor :: Game -> Direction -> Game
moveCursor game dir = game { cursor = newCursor }
  where
    (x, y) = cursor game
    newCursor = case dir of
      Up    -> (x, (y - 1 + size) `mod` size)
      Down  -> (x, (y + 1) `mod` size)
      T.Left  -> ((x - 1 + size) `mod` size, y)
      T.Right -> ((x + 1) `mod` size, y)
    size = length (board game)

undo :: Game -> Game
undo game = case prev game of
  Nothing -> game
  Just game' -> game'


