main :: IO ()
main = greet

data Creature = Player {
    name :: String
}


getPlayer :: IO Creature
getPlayer = do
    putStrLn "what is your name? "
    playerName <- getLine
    return (Player playerName)


greet :: IO ()
greet = do
    putStrLn "hello world"
    player <- getPlayer
    putStrLn ("Hello, " ++ name player)
    putStrLn "bye!"

