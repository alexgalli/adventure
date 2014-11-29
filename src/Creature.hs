module Creature (
    Creature(Creature),
    CreatureTemplate(CreatureTemplate),
    newCreature,
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

data CreatureTemplate = CreatureTemplate {
    tName :: String,
    tDescription :: String,
    tMaxHp :: Int
} deriving (Read, Show, Eq)

newCreature :: CreatureTemplate -> Creature
newCreature template = Creature {
    name = tName template,
    description = tDescription template,
    hp = tMaxHp template,
    maxHp = tMaxHp template
}
