import Control.Exception
import Data.List
import System.Directory
import System.IO
import Text.Read

import Creature
import Menu
import World

-- main menu

greet :: IO ()
greet = putStrLn "hello world"

--setPlayerName :: String -> IO (World)

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
    world <- loadWorld datafile
    world <- case world of
        Just w -> return w
        Nothing -> return $ newWorld datafile
    player <- return $ player world
    player <- case player of
        Just p -> do
            putStrLn $ "Welcome back to Adventure, " ++ name p
            return p
        Nothing -> do
            putStrLn "Welcome to Adventure!"
            p <- getCreature
            world <- return $ setPlayer p world
            saveWorld world
            return p
    runMenu mainMenu
    putStrLn "bye!"

