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
    [ LoopMenuItem "Greet me" greet
    , LoopMenuItem "Enter game" (runMenu gameMenu)
    , LoopMenuItem "Enter admin mode" (runMenu adminMenu)
    , Close "Exit"
    ]
