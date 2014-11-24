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
    [ MenuItem "Greet me" greet
    , MenuItem "Enter game" (runMenu gameMenu)
    , MenuItem "Enter admin mode" (runMenu adminMenu)
    , Close "Exit"
    ]
