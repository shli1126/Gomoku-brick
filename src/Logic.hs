{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use zipWith" #-}
module Logic (module Logic) where

import Type as T
import Data.List (transpose)

-- init has name conflict with Prelude.init
init1 :: Int -> [String] -> Game
init1 size names = Game
  { cursor = (size `div` 2, size `div` 2)
  , board = replicate size (replicate size (-1))
    -- 2 booms for each player
  , players = map (\name1 -> Player name1 2 0) names
  , prev = Nothing
  , nextPlayer = 0 -- TODO: randomize
  }

reset :: Game -> Game
reset game = game
  { board =  replicate size (replicate size (-1))
  , players = map (\player -> player { boomsLeft = 2 }) (players game)
  , prev = Nothing
  , nextPlayer = switchPlayer game
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
      T.Left  -> ((x - 1 + size) `mod` size, y)
      T.Right -> ((x + 1) `mod` size, y)
    size = length (board game)

undo :: Game -> Game
undo game = case prev game of
  Nothing -> game
  Just game' -> game'

dodo :: Game -> (Game, DoStatus)
dodo game = if board game !! y !! x == -1
  then if isWin newBoard
    then (game { board = newBoard, prev = Just game, players = newPlayers}, Win)
    else (game { board = newBoard, prev = Just game, nextPlayer = switchPlayer game }, Valid)
  else (game, Invalid)
  where
    (x, y) = cursor game
    newBoard = updateBoard (board game) (x, y) (nextPlayer game)
    newPlayers = map (\(player, playerIndex) -> if playerIndex == nextPlayer game
      then player {score = score player + 1 }
      else player) (zip (players game) [0..])

-- remove all pieces in a 2x2 square 
boom :: Game -> (Game, DoStatus)
boom game = if boomsLeft player > 0 && x < sizex-1 && y < sizey-1
  then (game { board = newBoard4, prev = Just game, players = newPlayers, nextPlayer = switchPlayer game}, Valid)
  else (game, Invalid)
  where
    (x, y) = cursor game
    newBoard4 = updateBoard newBoard3 (x+1,y+1) (-1)
    newBoard3 = updateBoard newBoard2 (x+1,y) (-1)
    newBoard2 = updateBoard newBoard1 (x,y+1) (-1)
    newBoard1 = updateBoard (board game) (x, y) (-1)

    newPlayers = map (\(player, playerIndex) -> if playerIndex == nextPlayer game
      then player {boomsLeft = boomsLeft player - 1 }
      else player) (zip (players game) [0..])
    player = players game !! nextPlayer game
    sizey = length (board game)
    sizex = length (head (board game))

switchPlayer :: Game -> Int
switchPlayer game = (currentPlayer + 1) `mod` playerCnt
        where 
            currentPlayer = nextPlayer game
            playerCnt = length (players game)

updateBoard :: [[a]] -> (Int, Int) -> a -> [[a]]
updateBoard board (x, y) player = take y board ++ [take x (board !! y) ++ [player] ++ drop (x + 1) (board !! y)] ++ drop (y + 1) board


-- check if there is a sequence of more than 5 same color
isWin :: Board -> Bool
isWin board = horizontalCheck board || verticalCheck board || diagonalCheck board

horizontalCheck :: Board -> Bool
horizontalCheck board = any (hasFiveConsecutive) board

verticalCheck :: Board -> Bool
verticalCheck board = horizontalCheck (transpose board)

diagonalCheck :: Board -> Bool
diagonalCheck board = any (hasFiveConsecutive) (diagonals1 board) || any (hasFiveConsecutive) (diagonals2 board)

hasFiveConsecutive :: [Int] -> Bool
hasFiveConsecutive (a:b:c:d:e:rest)
    | a == b && b == c && c == d && d == e && a >= 0 = True
    | otherwise = hasFiveConsecutive (b:c:d:e:rest)
hasFiveConsecutive _ = False

diagonals1 :: Board -> [[Int]]
diagonals1 [] = []
diagonals1 b = map (diagonal b) [0..(m + n - 2)]
    where
        n = length b
        m = length (head b)
        diagonal b t = [b !! y !! x | x <- [0..m], y <- [0..n], x < m, y < n, x + y == t]

diagonals2 :: Board -> [[Int]]
diagonals2 b = map (diagonal b) [-m+1..n-1]
    where
        n = length b
        m = length (head b)
        diagonal b t = [b !! y !! x | x <- [0..m], y <- [0..n], x < m, y < n, x - y == t]


testDiag1Board :: [[Int]]
testDiag1Board = [[-1,1,-1,-1,-1,-1],
                  [-1,-1,1,-1,-1,-1],
                  [-1,-1,-1,1,-1,-1],
                  [-1,-1,-1,-1,1,-1],
                  [-1,-1,-1,-1,-1,1],
                  [-1,-1,-1,-1,-1,-1]]

testDiag2Board :: [[Int]]
testDiag2Board = [[-1,-1, 1,-1,-1,-1],
                  [-1,-1,-1, 1,-1,-1],
                  [-1,-1,-1,-1, 1,-1],
                  [-1,-1,-1,-1,-1, 1],
                  [1,-1,-1,-1,-1,-1],
                  [-1,1,-1,-1,-1,-1]]

testDiag3Board :: [[Int]]
testDiag3Board = [[-1,-1, 1,-1,1,-1],
                  [-1,-1,-1, 1,-1,-1],
                  [-1,-1,1,-1, 1,-1],
                  [-1,1,-1,-1,-1, 1],
                  [1,-1,-1,-1,-1,-1],
                  [-1,-1,-1,-1,-1,-1]]

testDiag4Board :: [[Int]]
testDiag4Board = [[-1,-1, 1,-1,0,-1],
                  [-1,0,1, 1,1,1],
                  [-1,-1,1,-1, 1,-1],
                  [-1,1,-1,-1,0, 1],
                  [-1,-1,-1,-1,1,-1],
                  [-1,-1,-1,-1,1,-1]]


-- >>> isWin testDiag4Board
-- False

updateBoardTest :: [[Int]]
updateBoardTest = updateBoard testDiag4Board (1, 2) 4

-- >>> updateBoardTest
-- [[-1,-1,1,-1,0,-1],[-1,0,1,1,1,1],[-1,4,1,-1,1,-1],[-1,1,-1,-1,0,1],[-1,-1,-1,-1,1,-1],[-1,-1,-1,-1,1,-1]]
