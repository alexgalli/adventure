import Control.Exception
import System.Directory

import Creature
import Menu
import World

-- main menu

greet :: World -> IO World
greet world = do 
    case player world of
        Just p -> putStrLn $ "Hello there, " ++ name p
        Nothing -> putStrLn "Hello, stranger."
    return world

setPlayerName :: World -> IO (World)
setPlayerName world = do
    p <- getCreature
    return $ setPlayer p world

resetData :: World -> IO (World)
resetData world = do
    let df = datafile world
    removeFile $ df
    return $ newWorld df

mainMenu :: Menu.Menu World
mainMenu = getMenu
    [ ("Greet me", greet )
    , ("Set player name", setPlayerName)
    , ("Reset all user data", resetData)
    ]

-- main

defaultDatafile :: String
defaultDatafile = "data.txt"

main :: IO ()
main = do
    -- load/create world
    world <- loadWorld defaultDatafile
    world <- case world of
        Just w -> return w
        Nothing -> return $ newWorld defaultDatafile
    -- greeting
    case player world of
        Just p -> putStrLn $ "Welcome back to Adventure, " ++ name p
        Nothing -> do
            putStrLn "Welcome to Adventure!"
            -- create player and save
            p <- getCreature
            world <- return $ setPlayer p world
            saveWorld world
    -- main menu
    world <- runMenu mainMenu world
    -- save and quit
    saveWorld world
    putStrLn "bye!"
