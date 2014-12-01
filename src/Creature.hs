module Creature (
    Creature(Creature),
    CreatureFile(CreatureFile),
    CreatureTemplate(CreatureTemplate),
    newCreature,
    toCreature,
    toCreatureFile,
    index,
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

import Data.Unique
import Text.Printf

data Creature = Creature {
    index :: Unique,
    name :: String,
    description :: String,
    hp :: Int,
    maxHp :: Int
}

instance Eq Creature where
    (==) c1 c2 = index c1 == index c2

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

newCreature :: CreatureTemplate -> IO Creature
newCreature template = do
    u <- newUnique
    return Creature {
        index = u,
        name = tName template,
        description = tDescription template,
        hp = tMaxHp template,
        maxHp = tMaxHp template
    }

toCreature :: CreatureFile -> IO Creature
toCreature creatureFile = do
    u <- newUnique
    return Creature {
        index = u,
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
