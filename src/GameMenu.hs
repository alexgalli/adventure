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
        getMenuItemForEnemy enemy = CloseMenuItem (name enemy) Nothing (\_ -> return (Just enemy))
        enemyMenuItems = map getMenuItemForEnemy (enemies world)
        menuItems = enemyMenuItems ++ [Close "Don't select enemy"]

showTarget :: World -> String
showTarget world = 
    case target world of
        Just e -> "Currently targetting " ++ name e
        Nothing -> "Not targetting anyone"

-- menu items
targetEnemy :: World -> IO World
targetEnemy world = do
    enemy <- runMenu (enemyMenu world) Nothing
    case enemy of
        Just e -> do
            putStrLn $ "Now targetting " ++ name e
            return $ setTarget (Just e) world
        Nothing -> return world


listTarget :: World -> IO World
listTarget world = do
    -- TODO add Creature description
    putStrLn $ showTarget world
    return world

attack :: World -> IO World
attack world =
    case target world of
        Just t -> do
            putStrLn "Attacking the target!"
            return $ removeCreature t world
        Nothing -> do
            -- TODO add conditional menu showing
            putStrLn "There's no one to attack!"
            return world

showIfTargetted :: World -> Bool
showIfTargetted world =
    case target world of
        Just _ -> True
        Nothing -> False

gameMenu :: Menu.Menu World
gameMenu = getMenu
    showTarget
    [ LoopMenuItem "Target a monster" Nothing targetEnemy
    , LoopMenuItem "Describe target" (Just showIfTargetted) listTarget
    , LoopMenuItem "Attack" (Just showIfTargetted) attack
    , Close "Return to main menu"
    ]
