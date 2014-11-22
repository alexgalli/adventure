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
    putStrLn "What is your name? "
    playerName <- getLine
    return (Creature playerName)

writeCreature :: String -> Creature -> IO ()
writeCreature filename creature = do
    h <- openFile filename WriteMode
    hPrint h creature
    hClose h

loadCreature :: String -> IO (Maybe Creature)
loadCreature filename = do
    isFile <- doesFileExist datafile
    if isFile 
        then do
            h <- openFile filename ReadMode
            playerLine <- hGetLine h
            let player = readMaybe playerLine
            hClose h
            return player
        else
            return Nothing

-- main menu

greet :: IO ()
greet = putStrLn "hello world"

resetData :: String -> IO ()
resetData = removeFile

mainMenu :: Menu.Menu
mainMenu = getMenu
    [ ("Greet me", greet )
    , ("Reset all user data", resetData datafile)
    ]

-- main

datafile :: String
datafile = "data.txt"

main :: IO ()
main = do
    player <- loadCreature datafile
    player <- case player of
        Just p -> do
            putStrLn $ "Welcome back to Adventure, " ++ name p
            return p
        Nothing -> do
            putStrLn "Welcome to Adventure!"
            p <- getCreature
            writeCreature datafile p
            return p
    runMenu mainMenu
    putStrLn "bye!"

