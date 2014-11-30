module LibraryMenu (
    libraryMenu
) where

import Creature
import Library
import Menu

listAllMonsters :: Library -> IO Library
listAllMonsters library = do
    mapM_ (putStrLn . showCreatureTemplate) (templates library)
    return library

libraryMenu :: Menu Library
libraryMenu = getMenu
    (const "The monster library")
    [ LoopMenuItem 'l' "List all monsters" Nothing listAllMonsters
    , Close 'x' "Exit monster library" ]
