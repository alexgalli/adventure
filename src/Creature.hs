module Creature (
    Creature(Creature),
    CreatureFile(CreatureFile),
    CreatureTemplate(CreatureTemplate),
    newCreature,
    toCreature,
    toCreatureFile,
    name,
    description,
    hp,
    maxHp,
    fName,
    fDescription,
    fHp,
    fMaxHp,
    tName,
    tDescription,
    tMaxHp,
    showCreatureTemplate
) where

import Text.Printf

data Creature = Creature {
    name :: String,
    description :: String,
    hp :: Int,
    maxHp :: Int
} deriving (Read, Show, Eq)

data CreatureFile = CreatureFile {
    fName :: String,
    fDescription :: String,
    fHp :: Int,
    fMaxHp :: Int
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

toCreature :: CreatureFile -> Creature
toCreature creatureFile = Creature {
    name = fName creatureFile,
    description = fDescription creatureFile,
    hp = fHp creatureFile,
    maxHp = fMaxHp creatureFile
}

toCreatureFile :: Creature -> CreatureFile
toCreatureFile creature = CreatureFile {
    fName = name creature,
    fDescription = description creature,
    fHp = hp creature,
    fMaxHp = maxHp creature
}

showCreatureTemplate :: CreatureTemplate -> String
showCreatureTemplate template = printf "Name: %-20s  Description: %-65s  HP: %-4d" (tName template) (tDescription template) (tMaxHp template)
