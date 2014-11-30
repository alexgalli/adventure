module Library (
    Library,
    monsterLibrary
) where

import Creature

data Library = Library [CreatureTemplate] deriving (Show, Read, Eq)

monsterLibrary :: Library
monsterLibrary = Library
    [ CreatureTemplate {
        tName = "jackal",
        tDescription = "A small carnivorous mammal of the genus Canis",
        tMaxHp = 1
    }
    , CreatureTemplate {
        tName = "sewer rat",
        tDescription = "A brown rodent with glowing red eyes",
        tMaxHp = 1
    }
    , CreatureTemplate {
        tName = "newt",
        tDescription = "A newt is an aquatic amphibian of the family Salamandridae",
        tMaxHp = 1
    } ]
