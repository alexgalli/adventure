module Creature (
    Creature(Creature),
    name,
    getName
) where

-- Creature handling
data Creature = Creature {
    name :: String
} deriving (Read, Show, Eq)

getName :: Bool -> IO String
getName isPlayer = do
    if isPlayer
        then putStr "What is your name? "
        else putStr "What is the name of this creature? "
    getLine
