module Menu (
    Menu,
    MenuItem(MenuItem, Close),
    getMenu,
    runMenu
) where

import Data.List
import Text.Read

data MenuItem a     = MenuItem { description :: String, action :: a -> IO a } | Close { description :: String }
data Menu a         = Menu [(Integer, MenuItem a)]

instance Show (Menu a) where
    show (Menu items) = intercalate "\n" $ map showItem items
        where showItem (ix, item) = show ix ++ ". " ++ description item

getMenu :: [MenuItem a] -> Menu a
getMenu items = Menu $ zip [1..] items

getItem :: Maybe Int -> Menu a -> Maybe (MenuItem a)
getItem Nothing _ = Nothing
getItem (Just ix) (Menu items)
    | ix <= 0 = Nothing
    | ix > length items = Nothing
    | otherwise = Just (snd (items !! (ix - 1)))

-- user interface

prompt :: Menu a -> IO (MenuItem a)
prompt menu = do
    print menu
    let (Menu items) = menu
    let count = length items
    if count > 1
        then putStr $ "Choose [1-" ++ show count ++ "]: "
        else putStr "Choose [1]: "
    input <- getLine
    case getItem (readMaybe input) menu of
        Just item -> return item
        Nothing -> do
            putStrLn $ "'" ++ input ++ "' is not a valid selection."
            prompt menu

runMenu :: Menu a -> a -> IO a
runMenu menu a = do
    print "entering"
    item <- prompt menu
    case item of
        MenuItem _ _ -> do
            b <- action item a
            print "hi"
            runMenu menu b
        Close _ -> do
            print "here"
            -- TODO - figure out why this isn't returning (killing the loop)
            return a
