import Creature
import MainMenu
import Menu
import World

defaultDatafile :: String
defaultDatafile = "data.txt"

main :: IO ()
main = startWorld defaultDatafile
    >>= openingGreeting
    >>= runMenu mainMenu
    >>= exitAdventure

-- actions
startWorld :: String -> IO World
startWorld filename = do
    w <- loadWorld filename
    case w of
        Just world -> return world
        Nothing -> return $ newWorld defaultDatafile

openingGreeting :: World -> IO World
openingGreeting world =
    case player world of
        Just p -> do
            putStrLn $ "Welcome back to Adventure, " ++ name p
            return world
        Nothing -> do
            putStrLn "Welcome to Adventure!"
            -- create player and save
            playerName <- getName True
            let p = Creature playerName
            saveWorld $ setPlayer p world

exitAdventure :: World -> IO ()
exitAdventure world = do
    saveWorld world
    putStrLn "Until next time, adventurer."
