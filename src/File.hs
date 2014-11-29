module File (
    save,
    load
) where

import System.Directory
import System.IO

save :: Show a => String -> a -> IO a
save filename a = do
    h <- openFile filename WriteMode
    hPrint h a
    hClose h
    return a

load :: Read a => String -> IO (Maybe a)
load filename = do
    isFile <- doesFileExist filename
    if isFile
        then do
            hdl <- openFile filename ReadMode
            contents <- hGetLine hdl
            hClose hdl
            return $ Just (read contents)
        else
            return Nothing
