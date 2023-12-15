module Main (main) where

import Type
import UI
import Logic
import System.IO (hFlush, stdout)  -- Ensure this import is present
import Text.Read (readMaybe)


main :: IO Game
main = do
    putStrLn asciiArt
    size <- getBoardSize
    name1 <- getPlayerName "Player 1"
    name2 <- getPlayerName "Player 2"
    runUI size [name1, name2]


getBoardSize :: IO Int
getBoardSize = do
    putStrLn "Enter the size of the board (between 5 and 20):"
    hFlush stdout
    input <- getLine
    let maybeSize = readMaybe input :: Maybe Int
    case maybeSize of
        Just size | size >= 5 && size <= 20 -> return size
        _ -> do
            putStrLn "Invalid size. Please enter a number between 5 and 20."
            getBoardSize


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


asciiArt :: String
asciiArt =
 " _                                 \n\
  \| |__     ___     ___    _ __ ___  \n\
  \| '_ \\   / _ \\   / _ \\  | '_ ` _ \\ \n\
  \| |_) | | (_) | | (_) | | | | | | |\n\
  \|_.__/   \\___/   \\___/  |_| |_| |_|\n\
  \                                  "
