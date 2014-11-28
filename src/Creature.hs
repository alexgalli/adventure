module Creature (
    Creature(Creature),
    name,
    description,
    hp,
    maxHp
) where

-- Creature handling
data Creature = Creature {
    name :: String,
    description :: String,
    hp :: Int,
    maxHp :: Int
} deriving (Read, Show, Eq)
