module World (
    World,
    datafile,
    player,
    enemies,
    target,
    newWorld,
    setDataFile,
    setPlayer,
    addEnemy,
    clearEnemies,
    setTarget,
    saveWorld,
    loadWorld
) where

import System.Directory
import System.IO
import Creature

data World = World {
    datafile :: String,
    player   :: Maybe Creature,
    enemies  :: [Creature],
    target   :: Maybe Creature
} deriving (Show, Read)

newWorld :: String -> World
newWorld filename = World {
    datafile = filename,
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

setTarget :: Creature -> World -> World
setTarget creature world = world { target = Just creature }

-- file I/O
saveWorld :: World -> IO World
saveWorld world = do
    h <- openFile (datafile world) WriteMode
    hPrint h world
    hClose h
    return world

loadWorld :: String -> IO (Maybe World)
loadWorld filename = do
    isFile <- doesFileExist filename
    if isFile
        then do
            hdl <- openFile filename ReadMode
            contents <- hGetLine hdl
            hClose hdl
            return $ Just (read contents)
        else
            return Nothing
