import System.Directory
import System.IO

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

newtype Menu = Menu [(String, IO ())]

greet :: IO ()
greet = putStrLn "hello world"

mainMenu :: Menu
mainMenu = Menu
    [ ("Greet me", greet ) ]

getMenuLabels :: Menu -> [(Integer, String)]
getMenuLabels (Menu menu) = zip [1..] (map fst menu)

printMenu :: Menu -> IO ()
printMenu menu = do
    let labels = getMenuLabels menu
    let stringLabels = map (\label -> show (fst label) ++ ". " ++ snd label) labels
    mapM_ putStrLn stringLabels


-- main
main :: IO ()
main = do
    player <- getPlayer
    putStrLn ("Hello, " ++ name player)
    printMenu mainMenu
    putStrLn "bye!"

