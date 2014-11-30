module AdminMenu (
    adminMenu,
    getName,
    newPlayer
) where

import Data.List
import System.Directory

import Creature
import Library
import Menu
import World

getName :: IO String
getName = do
    putStr "What is your name? "
    getLine

setPlayerName :: World -> IO World
setPlayerName world = do
    playerName <- getName
    case player world of
        Just p -> return $ setPlayer (p { name = playerName }) world
        Nothing -> return $ setPlayer (newPlayer playerName) world

newPlayer :: String -> Creature
newPlayer n = Creature n "It's you!" 100 100

addNewEnemy :: World -> IO World
addNewEnemy world = do
    maybeTemplate <- runMenu (getListMenu (templates (library world)) tName "Select a creature to add") Nothing
    case maybeTemplate of
        Just t -> do
            putStrLn $ "Creating a new " ++ tName t
            return $ addEnemy (newCreature t) world
        Nothing -> return world

listEnemies :: World -> IO World
listEnemies world = do
    putStrLn $ "Enemies: " ++ intercalate ", " (map name (enemies world))
    return world

clearAllEnemies :: World -> IO World
clearAllEnemies world = return $ setTarget Nothing $ clearEnemies world

resetData :: World -> IO World
resetData world = do
    let df = datafile world
    removeFile df
    putStrLn "User data deleted"
    return $ newWorld df

adminMenu :: Menu.Menu World
adminMenu = getMenu
    (const "Administrate World")
    [ LoopMenuItem 's' "Set player name" Nothing setPlayerName
    , LoopMenuItem 'a' "Add a new enemy" Nothing addNewEnemy
    , LoopMenuItem 'l' "List enemies" Nothing listEnemies
    , LoopMenuItem 'c' "Clear all enemies" Nothing clearAllEnemies
    , LoopMenuItem 'r' "Reset all user data" Nothing resetData
    , Close 'x' "Exit"
    ]
