module Creature (
    Creature,
    name,
    getCreature,
    writeCreature,
    loadCreature
) where

import System.Directory
import System.IO
import Text.Read

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
    isFile <- doesFileExist filename
    if isFile 
        then do
            h <- openFile filename ReadMode
            playerLine <- hGetLine h
            let player = readMaybe playerLine
            hClose h
            return player
        else
            return Nothing
