module World (
    World,
    new,
    setDataFile,
    setPlayer,
    save,
    load
) where

import System.Directory
import System.IO
import Creature

data World = World {
    datafile :: Maybe String,
    player   :: Maybe Creature,
    enemies  :: [Creature]
} deriving (Show, Read)

new :: World
new = World {
    datafile = Nothing,
    player = Nothing,
    enemies = []
}

setDataFile :: String -> World -> World
setDataFile filename world = world { datafile = Just filename }

setPlayer :: Creature -> World -> World
setPlayer creature world = world { player = Just creature }

save :: World -> IO ()
save world = do
    filename <- case datafile world of
        Just f -> return f
        Nothing -> error "Datafile not specified"
    h <- openFile filename WriteMode
    hPrint h world
    hClose h

load :: String -> IO World
load filename = do
    isFile <- doesFileExist filename
    if isFile
        then do
            hdl <- openFile filename ReadMode
            contents <- hGetContents hdl
            hClose hdl
            return $ read contents
        else
            error "Could not find specified datafile"
