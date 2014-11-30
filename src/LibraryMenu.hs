module LibraryMenu (
    libraryMenu
) where

import Text.Read

import Creature
import Library
import Menu

createNewMonster :: Library -> IO Library
createNewMonster library = do
    putStr "What is the creature's name? "
    n <- getLine
    if not (checkName library n)
        then do
            putStrLn $ "Name '" ++ n ++ "' is already taken"
            return library
        else
            if null n
                then return library
                else
                    do
                    putStr "Describe this creature: "
                    d <- getLine
                    putStr "How many hit points does this creature have: "
                    hInput <- getLine
                    case readMaybe hInput :: Maybe Int of
                        Just h1 -> do
                            let t = CreatureTemplate {
                                tName = n,
                                tDescription = d,
                                tMaxHp = h1
                            }
                            putStrLn $ "Created new creature '" ++ n ++ "'"
                            return $ addTemplate library t
                        Nothing -> do
                            putStrLn $ "'" ++ hInput ++ "' is not a valid number"
                            return library

listAllMonsters :: Library -> IO Library
listAllMonsters library = do
    mapM_ (putStrLn . showCreatureTemplate) (templates library)
    return library

libraryMenu :: Menu Library
libraryMenu = getMenu
    (const "The monster library")
    [ LoopMenuItem 'c' "Create new monster" Nothing createNewMonster
    , LoopMenuItem 'l' "List all monsters" Nothing listAllMonsters
    , Close 'x' "Exit monster library" ]
