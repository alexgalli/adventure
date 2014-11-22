import Control.Exception
import Data.List
import System.Directory
import System.IO
import Text.Read

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

-- menu

type MenuItem     = (String, IO ())
type MenuListItem = (Integer, MenuItem)
type Menu         = [MenuListItem]

getMenu :: [MenuItem] -> Menu
getMenu = zip [1..]

showItem :: MenuListItem -> String
showItem (ix, (text, _)) = show ix ++ ". " ++ text

showMenu :: Menu -> String
showMenu menu = intercalate "\n" $ map showItem menu

printMenu :: Menu -> IO ()
printMenu menu = putStrLn $ showMenu menu

getItem :: Maybe Int -> Menu -> Maybe MenuItem
getItem Nothing _ = Nothing
getItem (Just ix) menu
    | ix <= 0 = Nothing
    | ix > length menu = Nothing
    | otherwise = Just (snd (menu !! (ix - 1)))

runMenu :: Menu -> IO ()
runMenu menu = do
    printMenu menu
    let count = length menu
    if count > 1
        then putStr $ "Choose [1-" ++ show count ++ "]: "
        else putStr "Choose [1]: "
    ln <- getLine
    case getItem (readMaybe ln) menu of
        Just li -> do
            putStrLn $ "You chose " ++ fst li
            snd li
        Nothing -> do
            putStrLn $ "'" ++ ln ++ "' is not a valid selection."
            runMenu menu

-- main menu

greet :: IO ()
greet = putStrLn "hello world"

mainMenu :: Menu
mainMenu = getMenu [ ("Greet me", greet ) ]

main :: IO ()
main = do
    player <- getPlayer
    putStrLn ("Hello, " ++ name player)
    runMenu mainMenu
    putStrLn "bye!"

