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

import Control.Applicative

import Creature
import File
import Library

data World = World {
    datafile :: String,
    library  :: Library,
    player   :: Maybe Creature,
    enemies  :: [Creature],
    target   :: Maybe Creature
}

newWorld :: String -> World
newWorld filename = World {
    datafile = filename,
    library = monsterLibrary,
    player = Nothing,
    enemies = [],
    target = Nothing
}

data WorldFile = WorldFile {
    fDatafile :: String,
    fLibrary  :: Library,
    fPlayer   :: Maybe CreatureFile,
    fEnemies  :: [CreatureFile],
    fTarget   :: Maybe CreatureFile
} deriving (Show, Read)

toWorld :: WorldFile -> IO World
toWorld worldFile = do
    -- TODO gotta figure out nested applicatives
    p <- case fPlayer worldFile of
        Just fp -> Just <$> toCreature fp
        Nothing -> return Nothing
    es <- mapM toCreature $ fEnemies worldFile
    t <- case fTarget worldFile of
        Just ft -> Just <$> toCreature ft
        Nothing -> return Nothing
    return World {
        datafile = fDatafile worldFile,
        library = fLibrary worldFile,
        player = p,
        enemies = es,
        target = t
    }

toWorldFile :: World -> WorldFile
toWorldFile world = WorldFile {
    fDatafile = datafile world,
    fLibrary = library world,
    fPlayer = toCreatureFile <$> player world,
    fEnemies = map toCreatureFile $ enemies world,
    fTarget = toCreatureFile <$> target world
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
saveWorld world = do
    save (datafile world) (toWorldFile world)
    return world

loadWorld :: String -> IO (Maybe World)
loadWorld filename = do
    worldFile <- load filename :: IO (Maybe WorldFile)
    case worldFile of
        Just wf -> Just <$> toWorld wf
        Nothing -> return Nothing
