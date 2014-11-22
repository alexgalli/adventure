import Control.Exception
import Data.List
import System.Directory
import System.IO
import Text.Read

import Menu

-- Creature handling
data Creature = Creature {
    name :: String
} deriving (Read, Show)

getCreature :: IO Creature
getCreature = do
    putStrLn "what is your name? "
    playerName <- getLine
    return (Creature playerName)

writeCreature :: String -> Creature -> IO ()
writeCreature filename creature = do
    h <- openFile filename WriteMode
    hPrint h creature
    hClose h

loadCreature :: String -> IO Creature
loadCreature filename = do
    h <- openFile filename ReadMode
    playerLine <- hGetLine h
    let player = read playerLine
    hClose h
    return player

datafile :: String
datafile = "data.txt"

getPlayer :: IO Creature
getPlayer = do
    isFile <- doesFileExist datafile
    player <- if isFile
        then
            loadCreature datafile
        else
            getCreature
    writeCreature datafile player
    return player

-- main menu

greet :: IO ()
greet = putStrLn "hello world"

mainMenu :: Menu.Menu
mainMenu = getMenu [ ("Greet me", greet ) ]

-- main
main :: IO ()
main = do
    player <- getPlayer
    putStrLn ("Hello, " ++ name player)
    runMenu mainMenu
    putStrLn "bye!"

