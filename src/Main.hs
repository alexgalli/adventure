main = greet

greet :: IO ()
greet = do
    putStrLn "hello world"
    putStrLn "what is your name? "
    name <- getLine
    putStrLn ("Hello, " ++ name)
    putStrLn "bye!"
