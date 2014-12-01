module Library (
    Library(Library),
    templates,
    addTemplate,
    checkName,
    playerTemplate,
    monsterLibrary
) where

import Creature

data Library = Library {
    templates :: [CreatureTemplate]
} deriving (Show, Read, Eq)

checkName :: Library -> String -> Bool
checkName library n =
    not $ any (\t -> tName t == n) (templates library)

addTemplate :: Library -> CreatureTemplate -> Library
addTemplate library t = library {
    templates = t : templates library
} 

playerTemplate :: CreatureTemplate
playerTemplate = CreatureTemplate {
    tName = "Player",
    tDescription = "A mighty adventurer!",
    tMaxHp = 100
}

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
