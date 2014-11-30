module World (
    World,
    datafile,
    library,
    player,
    enemies,
    target,
    newWorld,
    setDataFile,
    setPlayer,
    addEnemy,
    clearEnemies,
    setTarget,
    removeCreature,
    saveWorld,
    loadWorld
) where

import Creature
import File
import Library

data World = World {
    datafile :: String,
    library  :: Library,
    player   :: Maybe Creature,
    enemies  :: [Creature],
    target   :: Maybe Creature
} deriving (Show, Read)

newWorld :: String -> World
newWorld filename = World {
    datafile = filename,
    library = monsterLibrary,
    player = Nothing,
    enemies = [],
    target = Nothing
}

-- stateful operations
setDataFile :: String -> World -> World
setDataFile filename world = world { datafile = filename }

setPlayer :: Creature -> World -> World
setPlayer creature world = world { player = Just creature }

addEnemy :: Creature -> World -> World
addEnemy creature world = world { enemies = creature : enemies world }

clearEnemies :: World -> World
clearEnemies world = world { enemies = [] }

setTarget :: Maybe Creature -> World -> World
setTarget creature world = world { target = creature }

removeCreature :: Creature -> World -> World
removeCreature creature world =
    world {
        enemies = filter (/= creature) $ enemies world,
        target = if target world == Just creature then Nothing else target world
    }

-- file I/O
saveWorld :: World -> IO World
saveWorld world = save (datafile world) world

loadWorld :: String -> IO (Maybe World)
loadWorld = load 
