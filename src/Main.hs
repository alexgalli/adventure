import Control.Exception
import Data.List
import System.Directory
import System.IO
import Text.Read

import Creature
import Menu

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

-- TODO create world state object, pass it to runMenu to provide context to menu items

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

