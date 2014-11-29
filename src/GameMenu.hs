module GameMenu (
    gameMenu
) where

import Data.Char

import Creature
import Menu
import World

enemyMenu :: World -> Menu.Menu (Maybe Creature)
enemyMenu world =
    getMenu (const "Select enemy to target") menuItems
    where
        -- [a-z] -x, [A-Z] -X
        characterOptions = map chr ([97..119] ++ [121, 122] ++ [65..87] ++ [89, 90])
        enemiesWithCodes = zip characterOptions (enemies world)
        getMenuItemForEnemy (c, enemy) = CloseMenuItem c (name enemy) Nothing (\_ -> return (Just enemy))
        enemyMenuItems = map getMenuItemForEnemy enemiesWithCodes
        menuItems = enemyMenuItems ++ [Close 'x' "Don't target enemy"]

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
    [ LoopMenuItem 't' "Target a monster" Nothing targetEnemy
    , LoopMenuItem 'd' "Describe target" (Just showIfTargetted) listTarget
    , LoopMenuItem 'a' "Attack" (Just showIfTargetted) attack
    , Close 'x' "Return to main menu"
    ]
