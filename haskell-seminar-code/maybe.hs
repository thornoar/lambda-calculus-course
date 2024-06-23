import Data.List
import Data.Maybe
import Control.Monad

-- Adding failing conditions with Maybe

shiftListLeft :: Int -> [a] -> [a]
shiftListLeft 0 lst = lst
shiftListLeft _ [] = []
shiftListLeft 1 (a:as) = as ++ [a]
shiftListLeft n lst = (shiftListLeft 1) (shiftListLeft (n-1) lst)

shiftListRight :: Int -> [a] -> [a]
shiftListRight n = reverse . (shiftListLeft n) . reverse

varList :: [Char]
varList = shiftListRight 3 ['a'..'z']

(+++) :: (Eq a) => Maybe [a] -> Maybe [a] -> Maybe [a]
(+++) s1 s2
    | s1 == Nothing || s2 == Nothing = Nothing
    | otherwise = Just (fromJust s1 ++ fromJust s2)

getCombObjects' :: String -> Maybe [String]
getCombObjects' [] = Just []
getCombObjects' (')':rest) = Nothing
getCombObjects' ('(':')':rest) = getCombObjects' rest
getCombObjects' (var:rest)
    | var /= '(' = Just [[var]] +++ getCombObjects' rest
getCombObjects' str
    | n == 0 = Nothing
    | otherwise = Just [(take n str)] +++ getCombObjects' (drop n str)
    where
        findIndex :: String -> Int -> Int -> Int
        findIndex [] _ _ = 0
        findIndex ('(':rest) step count =
            findIndex rest (step+1) (count+1)
        findIndex (')':rest) step count
            | step == 0 = count
            | otherwise = findIndex rest (step-1) (count+1)
        findIndex (a:rest) step count = findIndex rest step (count+1)
        n :: Int
        n = findIndex str (-1) 1

formalVar' :: Char -> Maybe String
formalVar' var
    | elem var varList =
        let
            order = fromJust $ elemIndex var varList
            lst = repeat '\''
        in Just $ "v" ++ take order lst
    | otherwise = Nothing

toFormal' :: String -> Maybe String
toFormal' [] = Just []
toFormal' [var] = formalVar' var
toFormal' ('\\':var:'.':' ':rest) =
    Just "\\" +++ (formalVar' var) +++ Just ". " +++ (toFormal' rest)
toFormal' ('\\':var:',':rest) =
    Just "\\" +++ (formalVar' var) +++ Just ". (" +++ (toFormal' $ '\\':rest) +++ Just ")"
toFormal' expr
    | objects == Nothing = Nothing
    | (length justObjects == 0) = Just []
    | (length justObjects > 1) = Just "(" +++ (toFormal' operator) +++ (toFormal' operand) +++ Just ")"
    | otherwise =
        let
            object = head justObjects
            braced = ('(' == head object) && (')' == last object)
            trimmed = if braced then (tail $ init object) else object
            isLambda = (length trimmed > 0) && (head trimmed == '\\')
            inside = toFormal' trimmed
        in if isLambda then Just "(" +++ inside +++ Just ")" else inside
    where
        objects = getCombObjects' expr
        justObjects = fromJust objects
        operator = join $ init justObjects
        operand = last justObjects

toFormalIO' :: String -> IO ()
toFormalIO' str
    | formalStr == Nothing = putStrLn "error: incorrect syntax"
    | otherwise = putStrLn . id . fromJust $ formalStr
    where formalStr = toFormal' str
