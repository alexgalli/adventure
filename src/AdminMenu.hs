module AdminMenu (
    adminMenu
) where

import Data.List
import System.Directory

import Creature
import Menu
import World

setPlayerName :: World -> IO World
setPlayerName world = do
    playerName <- getName True
    case player world of
        Just p -> return $ setPlayer (p { name = playerName }) world
        Nothing -> return $ setPlayer (Creature playerName) world

addNewEnemy :: World -> IO World
addNewEnemy world = do
    enemyName <- getName False
    return $ addEnemy (Creature enemyName) world

listEnemies :: World -> IO World
listEnemies world = do
    putStrLn $ "Enemies: " ++ intercalate ", " (map name (enemies world))
    return world

clearAllEnemies :: World -> IO World
clearAllEnemies world = return $ clearEnemies world

resetData :: World -> IO World
resetData world = do
    let df = datafile world
    removeFile df
    putStrLn "User data deleted"
    return $ newWorld df

adminMenu :: Menu.Menu World
adminMenu = getMenu
    [ MenuItem "Set player name" setPlayerName
    , MenuItem "Add a new enemy" addNewEnemy
    , MenuItem "List enemies" listEnemies
    , MenuItem "Clear all enemies" clearAllEnemies
    , MenuItem "Reset all user data" resetData
    , Close "Exit"
    ]