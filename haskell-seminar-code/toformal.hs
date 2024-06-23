import Control.Monad
import Data.List
import Data.Maybe

-- Converting lambda strings from and to formal notation

shiftListLeft :: Int -> [a] -> [a]
shiftListLeft 0 lst = lst
shiftListLeft _ [] = []
shiftListLeft 1 (a : as) = as ++ [a]
shiftListLeft n lst = shiftListLeft 1 (shiftListLeft (n - 1) lst)

shiftListRight :: Int -> [a] -> [a]
shiftListRight n = reverse . (shiftListLeft n) . reverse

varList :: [Char]
varList = shiftListRight 3 ['a' .. 'z']

getCombObjects :: String -> [String]
getCombObjects [] = []
getCombObjects ('(' : ')' : rest) = getCombObjects rest
getCombObjects (var : rest)
  | var /= '(' = [var] : getCombObjects rest
getCombObjects str =
  take n str : getCombObjects (drop n str)
  where
    findIndex :: String -> Int -> Int -> Int
    findIndex [] _ _ = 0
    findIndex ('(' : rest) step count =
      findIndex rest (step + 1) (count + 1)
    findIndex (')' : rest) step count
      | step == 0 = count
      | otherwise = findIndex rest (step - 1) (count + 1)
    findIndex (a : rest) step count = findIndex rest step (count + 1)
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
toFormal ('\\' : var : '.' : ' ' : rest) =
  "\\" ++ formalVar var ++ ". " ++ toFormal rest
toFormal ('\\' : var : ',' : rest) =
  "\\" ++ formalVar var ++ ". (" ++ toFormal ('\\' : rest) ++ ")"
toFormal expr
  | null objects = []
  | length objects > 1 = "(" ++ toFormal operator ++ toFormal operand ++ ")"
  | otherwise =
    let
      object = head objects
      braced = ('(' == head object) && (')' == last object)
      trimmed = if braced then (tail . init) object else object
      isLambda = not (null trimmed) && head trimmed == '\\'
      inside = toFormal trimmed
    in if isLambda then "(" ++ inside ++ ")" else inside
  where
    objects = getCombObjects expr
    operator = join $ init objects
    operand = last objects

toFormalIO :: String -> IO ()
toFormalIO = putStrLn . toFormal

fromFormal :: String -> String
fromFormal = id
-- exercise for the attendants!
