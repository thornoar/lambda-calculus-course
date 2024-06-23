import Data.List
import Data.Maybe
import Control.Monad

-- Introduction to Haskell

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = (fib (n-2)) + (fib (n-1))

fibList :: [Int]
fibList = map fib [0..]

shiftListLeft :: Int -> [a] -> [a]
shiftListLeft 0 lst = lst
shiftListLeft _ [] = []
shiftListLeft 1 (a:as) = as ++ [a]
shiftListLeft n lst = (shiftListLeft 1) (shiftListLeft (n-1) lst)

shiftListRight :: Int -> [a] -> [a]
shiftListRight n = reverse . (shiftListLeft n) . reverse

-- Converting lambda strings from and to formal notation

varList :: [Char]
varList = shiftListRight 3 ['a'..'z']

getBraceObjects :: String -> [String]
getBraceObjects [] = []
getBraceObjects ('(':')':rest) = getBraceObjects rest
getBraceObjects (var:rest)
    | var /= '(' = [var] : getBraceObjects rest
getBraceObjects str =
    (take n str) : getBraceObjects (drop n str)
    where
        findIndex :: String -> Int -> Int -> Int
        findIndex [] _ _ = 0
        findIndex ('(':rest') step count =
            findIndex rest' (step+1) (count+1)
        findIndex (')':rest') step count
            | step == 0 = count
            | otherwise = findIndex rest' (step-1) (count+1)
        findIndex (a:rest') step count = findIndex rest' step (count+1)
        n :: Int
        n = findIndex str (-1) 1

formalVar :: Char -> String
formalVar var
    | elem var varList =
        let
            order = fromJust $ elemIndex var varList
            lst = repeat '\''
        in "v" ++ take order lst
    | otherwise = [var]

toFormal :: String -> String
toFormal [] = []
toFormal [var] = formalVar var
toFormal ('(':var:')':[]) = formalVar var
toFormal ('\\':var:'.':' ':rest) =
    "\\" ++ (formalVar var) ++ ". " ++ (toFormal rest)
toFormal ('\\':var:',':rest) =
    "\\" ++ (formalVar var) ++ ". (" ++ (toFormal $ '\\':rest) ++ ")"
toFormal expr
    | (length objects == 0) = []
    | (length objects > 1) = "(" ++ (toFormal operator) ++ (toFormal operand) ++ ")"
    | otherwise =
        let
            object = head objects
            braced = ('(' == head object) && (')' == last object)
            trimmed = if braced then (tail $ init object) else object
            isLambda = (length trimmed > 0) && (head trimmed == '\\')
            inside = toFormal trimmed
        in if isLambda then "(" ++ inside ++ ")" else inside
    where
        objects = getBraceObjects expr
        operator = join $ init objects
        operand = last objects
        -- combination

(+++) :: Maybe String -> Maybe String -> Maybe String
(+++) s1 s2
    | s1 == Nothing || s2 == Nothing = Nothing
    | otherwise = Just (fromJust s1 ++ fromJust s2)

toFormalMaybe :: String -> Maybe String
-- toFormalMaybe "x" = "v"
-- toFormalMaybe "y" = "v'"
-- toFormalMaybe "z" = "v''"
-- toFormalMaybe "a" = "v'''"
-- toFormalMaybe "b" = "v''''"
-- toFormalMaybe "c" = "v'''''"
-- toFormalMaybe "f" = "v''''''"
-- toFormalMaybe "g" = "v'''''''"
-- toFormalMaybe "h" = "v''''''''"
toFormalMaybe [] = Just []
toFormalMaybe [var] = Just $ formalVar var
toFormalMaybe ('\\':var:',':rest) = (Just "\\") +++ (toFormalMaybe [var]) +++ (Just "(") +++ (toFormalMaybe $ '\\':rest) +++ (Just ")")
toFormalMaybe ('\\':var:'.':' ':rest) = (Just "\\") +++ (toFormalMaybe [var]) +++ (Just "(") +++ (toFormalMaybe rest) +++ (Just ")")
toFormalMaybe (var1:var2:rest) = (Just "(") +++ (toFormalMaybe [var1]) +++ (toFormalMaybe [var2]) +++ (Just ")") +++ (toFormalMaybe rest)
-- toFormalMaybe _ = Nothing

toFormal' :: String -> IO ()
toFormal' = putStrLn . id . toFormal
