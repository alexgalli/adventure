module World (
    World,
    datafile,
    player,
    enemies,
    newWorld,
    setDataFile,
    setPlayer,
    saveWorld,
    loadWorld
) where

import System.Directory
import System.IO
import Creature

data World = World {
    datafile :: String,
    player   :: Maybe Creature,
    enemies  :: [Creature]
} deriving (Show, Read)

newWorld :: String -> World
newWorld filename = World {
    datafile = filename,
    player = Nothing,
    enemies = []
}

setDataFile :: String -> World -> World
setDataFile filename world = world { datafile = filename }

setPlayer :: Creature -> World -> World
setPlayer creature world = world { player = Just creature }

saveWorld :: World -> IO ()
saveWorld world = do
    h <- openFile (datafile world) WriteMode
    hPrint h world
    hClose h

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
