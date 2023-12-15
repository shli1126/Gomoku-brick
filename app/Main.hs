module Main (main) where

import Type
import UI
import Logic
import System.IO (hFlush, stdout)  -- Ensure this import is present


main :: IO Game
main = do
    name1 <- getPlayerName "Player 1"
    name2 <- getPlayerName "Player 2"
    runUI [name1, name2]

getPlayerName :: String -> IO String
getPlayerName player = do
    putStrLn $ "Enter name for " ++ player ++ " (max 20 characters):"
    hFlush stdout
    name <- getLine
    if length name > 20 || null name
        then do
            putStrLn "Name cannot be empty or longer than 20 characters."
            getPlayerName player
        else return name









