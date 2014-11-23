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
data Menu         = Menu [MenuListItem]

instance Show Menu where
    show (Menu items) = intercalate "\n" $ map showItem items
        where showItem (ix, (text, _)) = show ix ++ ". " ++ text

getMenu :: [MenuItem] -> Menu
getMenu items = Menu $ zip [1..] items

getItem :: Maybe Int -> Menu -> Maybe MenuItem
getItem Nothing _ = Nothing
getItem (Just ix) (Menu items)
    | ix <= 0 = Nothing
    | ix > length items = Nothing
    | otherwise = Just (snd (items !! (ix - 1)))

runMenu :: Menu -> IO ()
runMenu menu = do
    print menu
    let (Menu items) = menu
    let count = length items
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
