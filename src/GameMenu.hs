module GameMenu (
    gameMenu
) where

import Creature
import Menu
import World

enemyMenu :: World -> Menu.Menu (Maybe Creature)
enemyMenu world =
    getMenu (const "Select enemy to target") menuItems
    where
        getMenuItemForEnemy enemy = CloseMenuItem (name enemy) (\_ -> return (Just enemy))
        enemyMenuItems = map getMenuItemForEnemy (enemies world)
        menuItems = enemyMenuItems ++ [Close "Don't select enemy"]

targetEnemy :: World -> IO World
targetEnemy world = do
    enemy <- runMenu (enemyMenu world) Nothing
    case enemy of
        Just e -> do
            putStrLn $ "Now targetting " ++ name e
            return $ setTarget e world
        Nothing -> return world

showTarget :: World -> String
showTarget world = 
    case target world of
        Just e -> "Currently targetting " ++ name e
        Nothing -> "Not targetting anyone"

listTarget :: World -> IO World
listTarget world = do
    putStrLn $ showTarget world
    return world

gameMenu :: Menu.Menu World
gameMenu = getMenu
    showTarget
    [ LoopMenuItem "Target a monster" targetEnemy
    , LoopMenuItem "Describe target" listTarget
    , Close "Return to main menu"
    ]
