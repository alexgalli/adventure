module MainMenu (
    mainMenu
) where

import AdminMenu
import Creature
import GameMenu
import Menu
import World

greet :: World -> IO World
greet world = do 
    case player world of
        Just p -> putStrLn $ "Hello there, " ++ name p
        Nothing -> putStrLn "Hello, stranger."
    return world

mainMenu :: Menu.Menu World
mainMenu = getMenu
    (const "Main Menu")
    [ LoopMenuItem 'h' "Greet me" Nothing greet
    , LoopMenuItem 'g' "Enter game" Nothing (runMenu gameMenu)
    , LoopMenuItem 'a' "Enter admin mode" Nothing (runMenu adminMenu)
    , Close 'x' "Exit"
    ]
