module Menu (
    Menu,
    MenuItem(LoopMenuItem, CloseMenuItem, Close),
    getMenu,
    runMenu
) where

import Data.List
import Text.Read

type MenuAction a = a -> IO a

data MenuItem a
    = LoopMenuItem { description :: String, action :: MenuAction a}
    | CloseMenuItem { description :: String, action :: MenuAction a}
    | Close { description :: String }

data Menu a = Menu {
    showTitle :: a -> String,
    items :: [(Integer, MenuItem a)]
}

instance Show (Menu a) where
    show menu = intercalate "\n" $ map showItem (items menu)
        where showItem (ix, item) = show ix ++ ". " ++ description item

getMenu :: (a -> String) -> [MenuItem a] -> Menu a
getMenu st i = Menu st $ zip [1..] i

getItem :: Maybe Int -> Menu a -> Maybe (MenuItem a)
getItem Nothing _ = Nothing
getItem (Just ix) menu
    | ix <= 0 = Nothing
    | ix > length (items menu) = Nothing
    | otherwise = Just (snd (items menu !! (ix - 1)))

-- user interface

prompt :: Menu a -> a -> IO (MenuItem a)
prompt menu a = do
    putStrLn ""
    putStrLn $ showTitle menu a
    print menu
    let count = length (items menu)
    if count > 1
        then putStr $ "Choose [1-" ++ show count ++ "]: "
        else putStr "Choose [1]: "
    input <- getLine
    case getItem (readMaybe input) menu of
        Just item -> return item
        Nothing -> do
            putStrLn $ "'" ++ input ++ "' is not a valid selection."
            prompt menu a

runMenu :: Menu a -> a -> IO a
runMenu menu a = do
    item <- prompt menu a
    putStrLn ""
    case item of
        LoopMenuItem {} -> do
            b <- action item a
            runMenu menu b
        CloseMenuItem {} -> action item a
        Close {} -> return a
