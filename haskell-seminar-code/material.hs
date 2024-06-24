import Control.Monad
import Data.List
import Data.Maybe

-- $basic
-- ┌────────────────────┐
-- │ Basic introduction │
-- └────────────────────┘

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibList :: [Int]
fibList = map fib [0 ..]

square :: Float -> Float
square x = x * x

prefixLength :: (Eq a) => [a] -> [a] -> Int
prefixLength [] _ = 0
prefixLength _ [] = 0
prefixLength (x : xs) (y : ys)
  | x == y = 1 + prefixLength xs ys
  | otherwise = 0

-- $conversion
-- ┌─────────────────────────────────────────┐
-- │ Converting formal and informal notation │
-- └─────────────────────────────────────────┘

varSet :: [String]
varSet = ['v' : replicate n '\'' | n <- [0 ..]]

varSet' :: [Char]
varSet' = ['x', 'y', 'z', 'u', 'v', 'w', 'a', 'b', 'c']

getCombObjects :: String -> [String]
getCombObjects [] = []
getCombObjects ('(' : ')' : rest) = getCombObjects rest
getCombObjects ('v' : rest) = ('v' : replicate n '\'') : getCombObjects (drop n rest)
  where
    n = prefixLength (repeat '\'') rest
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
  | var `elem` varSet' =
      let order = fromJust $ elemIndex var varSet'
       in varSet !! order
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
      let object = head objects
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

-- $maybe
-- ┌─────────────────────────────────────┐
-- │ Adding failing conditions viz Maybe │
-- └─────────────────────────────────────┘

(+++) :: (Eq a) => Maybe [a] -> Maybe [a] -> Maybe [a]
(+++) s1 s2
  | isNothing s1 || isNothing s2 = Nothing
  | otherwise = Just (fromJust s1 ++ fromJust s2)

getCombObjects' :: String -> Maybe [String]
getCombObjects' [] = Just []
getCombObjects' (')' : rest) = Nothing
getCombObjects' ('(' : ')' : rest) = getCombObjects' rest
getCombObjects' ('v' : rest) = Just ['v' : replicate n '\''] +++ getCombObjects' (drop n rest)
  where
    n = prefixLength (repeat '\'') rest
getCombObjects' (var : rest)
  | var /= '(' = if var `elem` varSet' then Just [[var]] +++ getCombObjects' rest else Nothing
getCombObjects' str
  | n == 0 = Nothing
  | otherwise = Just [take n str] +++ getCombObjects' (drop n str)
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

formalVar' :: Char -> Maybe String
formalVar' var
  | var `elem` varSet' =
      let order = fromJust $ elemIndex var varSet'
       in Just (varSet !! order)
  | otherwise = Nothing

toFormal' :: String -> Maybe String
toFormal' [] = Just []
toFormal' [var] = formalVar' var
toFormal' ('\\' : var : '.' : ' ' : rest) =
  Just "\\" +++ formalVar' var +++ Just ". " +++ toFormal' rest
toFormal' ('\\' : var : ',' : rest) =
  Just "\\" +++ formalVar' var +++ Just ". (" +++ toFormal' ('\\' : rest) +++ Just ")"
toFormal' expr
  | isNothing objects = Nothing
  | null justObjects = Just []
  | length justObjects > 1 = Just "(" +++ toFormal' operator +++ toFormal' operand +++ Just ")"
  | otherwise =
      let object = head justObjects
          braced = ('(' == head object) && (')' == last object)
          trimmed = if braced then tail $ init object else object
          isLambda = not (null trimmed) && (head trimmed == '\\')
          inside = toFormal' trimmed
       in if isLambda then Just "(" +++ inside +++ Just ")" else inside
  where
    objects = getCombObjects' expr
    justObjects = fromJust objects
    operator = join $ init justObjects
    operand = last justObjects

toFormalIO' :: String -> IO ()
toFormalIO' str
  | isNothing formalStr = putStrLn "error: incorrect syntax"
  | otherwise = putStrLn . fromJust $ formalStr
  where
    formalStr = toFormal' str

-- $model
-- ┌───────────────────────────────┐
-- │ Modelling the lambda calculus │
-- └───────────────────────────────┘

data Lambda = Var Int | Abst Int Lambda | Appl Lambda Lambda deriving (Read, Show)

instance Eq Lambda where
  (==) :: Lambda -> Lambda -> Bool
  (==) (Var n) (Var n')
    | n == n' = True
    | otherwise = False
  (==) (Abst n l) (Abst n' l') = (n == n') && (l == l')
  (==) (Appl l1 l2) (Appl l1' l2') = (l1 == l1') && (l2 == l2')
  (==) _ _ = False

parse :: String -> Maybe Lambda
parse [] = Nothing
parse [var]
  | var `elem` varSet' = Just $ Var (fromJust $ elemIndex var varSet')
parse str
  | (head str == 'v') && (tail str == replicate n '\'') = Just (Var n)
  where
    n = length str - 1
parse [var] = Nothing
parse ('\\' : var : rest)
  | var == 'v' =
      let findEnd :: String -> Int -> (Int, Bool)
          findEnd (',' : str) m = (m, False)
          findEnd ('.' : ' ' : str) m = (m, True)
          findEnd ('\'' : str) m = findEnd str (m + 1)
          findEnd _ _ = (-1, False)
          (n, dot) = findEnd rest 0
          parsedRest = parse $ if dot then drop (n + 2) rest else '\\' : drop (n + 1) rest
       in if n == (-1) || isNothing parsedRest then Nothing else Just $ Abst n (fromJust parsedRest)
  | var `notElem` varSet' = Nothing
  | head rest == ',' =
      let parsedRest = parse $ '\\' : tail rest
       in if isNothing parsedRest
            then Nothing
            else Just $ Abst (fromJust $ elemIndex var varSet') (fromJust parsedRest)
  | head rest == '.' =
      let parsedRest = parse (drop 2 rest)
       in if isNothing parsedRest
            then Nothing
            else Just $ Abst (fromJust $ elemIndex var varSet') (fromJust parsedRest)
parse str
  | isNothing objects = Nothing
  | null justObjects = Nothing
  | length justObjects == 1 =
    let object = head justObjects
        braced = ('(' == head object) && (')' == last object)
        trimmed = if braced then tail $ init object else object
     in parse trimmed
  | otherwise =
    let operator = parse $ join (init justObjects)
        operand = parse $ last justObjects
     in if isNothing operator || isNothing operand
        then Nothing
        else Just $ Appl (fromJust operator) (fromJust operand)
  where
    objects = getCombObjects' str
    justObjects = fromJust objects

embraceAbst :: (Lambda -> String) -> Lambda -> String
embraceAbst f (Abst n l) = "(" ++ f (Abst n l) ++ ")"
embraceAbst f l = f l

unparse :: Lambda -> String
unparse (Var n) = varSet !! n
unparse (Abst n l) = "\\" ++ unparse (Var n) ++ ". " ++ embraceAbst unparse l
unparse (Appl l1 l2) = "(" ++ embraceAbst unparse l1 ++ embraceAbst unparse l2 ++ ")"

unparse' :: Lambda -> String
unparse' (Var n) = [varSet' !! n]
unparse' (Abst n (Abst m l)) = "\\" ++ unparse' (Var n) ++ "," ++ (tail $ unparse' (Abst m l))
unparse' (Abst n l) = "\\" ++ unparse' (Var n) ++ ". " ++ embraceAbst unparse' l
unparse' (Appl (Appl l1 l2) l3) = embraceAbst unparse' l1 ++ embraceAbst unparse' l2 ++ embraceAbst unparse' l3
unparse' (Appl l1 l2) = "(" ++ embraceAbst unparse' l1 ++ embraceAbst unparse' l2 ++ ")"
