module Menu (
    Menu,
    MenuItem,
    getMenu,
    runMenu
) where

import Data.List
import Text.Read

type MenuItem     = (String, IO ())
type MenuListItem = (Integer, MenuItem)
type Menu         = [MenuListItem]

getMenu :: [MenuItem] -> Menu
getMenu = zip [1..]

showItem :: MenuListItem -> String
showItem (ix, (text, _)) = show ix ++ ". " ++ text

showMenu :: Menu -> String
showMenu menu = intercalate "\n" $ map showItem menu

printMenu :: Menu -> IO ()
printMenu menu = putStrLn $ showMenu menu

getItem :: Maybe Int -> Menu -> Maybe MenuItem
getItem Nothing _ = Nothing
getItem (Just ix) menu
    | ix <= 0 = Nothing
    | ix > length menu = Nothing
    | otherwise = Just (snd (menu !! (ix - 1)))

runMenu :: Menu -> IO ()
runMenu menu = do
    printMenu menu
    let count = length menu
    if count > 1
        then putStr $ "Choose [1-" ++ show count ++ "]: "
        else putStr "Choose [1]: "
    ln <- getLine
    case getItem (readMaybe ln) menu of
        Just li -> do
            putStrLn $ "You chose " ++ fst li
            snd li
        Nothing -> do
            putStrLn $ "'" ++ ln ++ "' is not a valid selection."
            runMenu menu
