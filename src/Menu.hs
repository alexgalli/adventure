module Menu (
    Menu,
    MenuItem(LoopMenuItem, CloseMenuItem, Close),
    getMenu,
    runMenu,
    getListMenu
) where

import Data.Char
import Data.List

type MenuAction a = a -> IO a
type Predicate a = a -> Bool

-- menu item
data MenuItem a
    = LoopMenuItem  { code :: Char, description :: String, displayAction :: Maybe (Predicate a), action :: MenuAction a}
    | CloseMenuItem { code :: Char, description :: String, displayAction :: Maybe (Predicate a), action :: MenuAction a}
    | Close         { code :: Char, description :: String }

instance Show (MenuItem a) where
    show menuItem = "[" ++ [code menuItem] ++ "] " ++ description menuItem

filterItem :: a -> MenuItem a -> Bool
filterItem _ (Close {}) = True
filterItem a menuItem =
    case displayAction menuItem of
        Just da -> da a
        Nothing -> True

-- menu
type TitleAction a = a -> String

data Menu a = Menu {
    showTitle :: TitleAction a,
    items :: [MenuItem a]
}

instance Show (Menu a) where
    show menu = intercalate "\n" $ map show (items menu)

getMenu :: TitleAction a -> [MenuItem a] -> Menu a
getMenu titleAction menuItems
    | null menuItems                                          = Menu titleAction [Close 'x' "Exit"]
    | length menuItems /= (length . nub $ map code menuItems) = error "Duplicate menu item codes in your menu"
    | otherwise                                               = Menu titleAction menuItems

getItem :: Char -> Menu a -> Maybe (MenuItem a)
getItem c menu =
    if null matchingItems
        then Nothing
        else Just (head matchingItems)
    where matchingItems = filter (\menuItem -> c == code menuItem) (items menu)

filterMenu :: Menu a -> a -> Menu a 
filterMenu menu a =
    getMenu (showTitle menu) $ filter (filterItem a) (items menu)

-- user interface

prompt :: Menu a -> a -> IO (MenuItem a)
prompt initialMenu a = do
    putStrLn ""
    let menu = filterMenu initialMenu a
    putStrLn $ showTitle menu a
    print menu
    putStr $ "Choose [" ++ map code (items menu)  ++ "]: "
    input <- getLine
    if null input
        then prompt menu a
        else 
            case getItem (head input) menu of
                Just item -> return item
                Nothing -> do
                    putStrLn $ "'" ++ [head input] ++ "' is not a valid selection."
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

-- generic list menu
getListMenu :: [a] -> (a -> String) -> String -> Menu.Menu (Maybe a)
getListMenu listItems getLabel title =
    getMenu (const title) menuItems
    where
        -- [a-z] -x, [A-Z] -X
        characterOptions = map chr ([97..119] ++ [121, 122] ++ [65..87] ++ [89, 90])
        itemsWithCodes = zip characterOptions listItems
        getMenuItem (c, i) = CloseMenuItem c (getLabel i) Nothing (\_ -> return (Just i))
        menuItems = (map getMenuItem itemsWithCodes) ++ [Close 'x' "Don't target enemy"]
