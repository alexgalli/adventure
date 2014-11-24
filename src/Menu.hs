module Menu (
    Menu,
    MenuItem,
    getMenu,
    runMenu
) where

import Data.List
import Text.Read

type MenuItem a     = (String, a -> IO a)
type MenuListItem a = (Integer, MenuItem a)
data Menu a         = Menu [MenuListItem a]

instance Show (Menu a) where
    show (Menu items) = intercalate "\n" $ map showItem items
        where showItem (ix, (text, _)) = show ix ++ ". " ++ text

getMenu :: [MenuItem a] -> Menu a
getMenu items = Menu $ zip [1..] items

getItem :: Maybe Int -> Menu a -> Maybe (MenuItem a)
getItem Nothing _ = Nothing
getItem (Just ix) (Menu items)
    | ix <= 0 = Nothing
    | ix > length items = Nothing
    | otherwise = Just (snd (items !! (ix - 1)))

runMenu :: Menu a -> a -> IO a
runMenu menu a = do
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
            snd li a
        Nothing -> do
            putStrLn $ "'" ++ ln ++ "' is not a valid selection."
            runMenu menu a
