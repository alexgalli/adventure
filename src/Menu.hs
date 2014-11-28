module Menu (
    Menu,
    MenuItem(LoopMenuItem, CloseMenuItem, Close),
    getMenu,
    runMenu
) where

import Data.List
import Text.Read

type MenuAction a = a -> IO a
type Predicate a = a -> Bool

data MenuItem a
    = LoopMenuItem { description :: String, displayAction :: Maybe (Predicate a), action :: MenuAction a}
    | CloseMenuItem { description :: String, displayAction :: Maybe (Predicate a), action :: MenuAction a}
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

filterMenu :: Menu a -> a -> Menu a 
filterMenu menu b =
    getMenu (showTitle menu) $ filter filterItem (map snd (items menu))
    where
        filterItem menuItem =
            case menuItem of
                Close _ -> True
                _ -> case displayAction menuItem of
                    Just da -> da b
                    Nothing -> True

-- user interface

prompt :: Menu a -> a -> IO (MenuItem a)
prompt initialMenu a = do
    putStrLn ""
    let menu = filterMenu initialMenu a
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
