import System.Directory
import System.IO

main :: IO ()
main = greet

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
    handle <- openFile filename WriteMode
    hPrint handle creature
    hClose handle

loadCreature :: String -> IO Creature
loadCreature filename = do
    handle <- openFile filename ReadMode
    playerLine <- hGetLine handle
    let player = read playerLine
    hClose handle
    return player

datafile :: String
datafile = "data.txt"

greet :: IO ()
greet = do
    putStrLn "hello world"
    isFile <- doesFileExist datafile
    player <- if isFile
        then
            loadCreature datafile
        else
            getCreature
    putStrLn ("Hello, " ++ name player)
    writeCreature datafile player
    putStrLn "bye!"

