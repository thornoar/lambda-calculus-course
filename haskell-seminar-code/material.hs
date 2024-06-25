import Control.Monad
import Data.List (elemIndex, nub)
import Data.Maybe
import Data.Set (Set, delete, empty, insert, intersection, member, notMember, singleton, union)

-- $basic
-- ┌────────────────────┐
-- │ Basic introduction │
-- └────────────────────┘

fib :: Variable -> Variable
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

checkCondition :: (a -> Bool) -> a -> Maybe a
checkCondition f a = if f a then Just a else Nothing

-- [TODO]
-- union :: (Eq a) => [a] -> [a] -> [a]
-- union [] bs = bs
-- union (a:as) bs
--   | a `elem` bs = union as bs
--   | otherwise = a : union as bs

-- [TODO]
-- elemIndex :: (Eq a) => a -> [a] -> Maybe Variable
-- elemIndex a as
--   | a `notElem` as = Nothing
--   | otherwise = Just $ findIndex a as 0
--   where
--     findIndex :: (Eq a) => a -> [a] -> Variable -> Variable
--     findIndex a (b:bs) n
--       | a == b = n
--       | otherwise = findIndex a bs (n+1)

getCombinedTerms :: String -> [String]
getCombinedTerms [] = []
getCombinedTerms (' ' : rest) = getCombinedTerms rest
getCombinedTerms ('(' : ')' : rest) = getCombinedTerms rest
getCombinedTerms (var : '\'' : rest) = (var : replicate (n + 1) '\'') : getCombinedTerms (drop n rest)
  where
    n = prefixLength (repeat '\'') rest
getCombinedTerms (var : rest)
  | var /= '(' = [var] : getCombinedTerms rest
getCombinedTerms str = take n str : getCombinedTerms (drop n str)
  where
    findIndex :: String -> Variable -> Variable -> Variable
    findIndex [] _ _ = 1
    findIndex ('(' : rest) step count = findIndex rest (step + 1) (count + 1)
    findIndex (')' : rest) step count
      | step == 0 = count
      | otherwise = findIndex rest (step - 1) (count + 1)
    findIndex (a : rest) step count = findIndex rest step (count + 1)
    n :: Variable
    n = findIndex str (-1) 1

(...) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(...) g f x = f x >>= g

infixr 9 ...

(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) f g x = f x && g x

-- $conversion
-- ┌─────────────────────────────────────────┐
-- │ Converting formal and informal notation │
-- └─────────────────────────────────────────┘

varSet :: [String]
varSet = ['v' : replicate n '\'' | n <- [0 ..]]

varSet' :: [Char]
varSet' = ['x', 'y', 'z', 'w', 'u', 't', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']

toFormal :: String -> String
toFormal [] = []
toFormal [var]
  | var `elem` varSet' =
      let order = fromJust $ elemIndex var varSet'
       in varSet !! order
  | otherwise = [var]
toFormal ('\\' : var : '.' : ' ' : rest) = "(\\" ++ toFormal [var] ++ toFormal rest ++ ")"
toFormal ('\\' : var : ',' : rest) = "(\\" ++ toFormal [var] ++ toFormal ('\\' : rest) ++ ")"
toFormal expr
  | null objects = []
  | length objects == 1 =
      let object = head objects
          braced = ('(' == head object) && (')' == last object)
          object' = if braced then (tail . init) object else object
       in toFormal object'
  | otherwise =
      let operator = join $ init objects
          operand = last objects
       in "(" ++ toFormal operator ++ toFormal operand ++ ")"
  where
    objects = getCombinedTerms expr

-- [TODO]
-- fromFormal :: String -> String
-- fromFormal = id

-- $maybe
-- ┌─────────────────────────────────────┐
-- │ Adding failing conditions via Maybe │
-- └─────────────────────────────────────┘

io :: Maybe String -> IO ()
io Nothing = putStrLn "error: invalid input"
io (Just str) = putStrLn str

(+++) :: (Eq a) => Maybe [a] -> Maybe [a] -> Maybe [a]
(+++) s1 s2
  | isNothing s1 || isNothing s2 = Nothing
  | otherwise = Just (fromJust s1 ++ fromJust s2)

toFormalMaybe :: String -> Maybe String
toFormalMaybe [] = Nothing
toFormalMaybe [var]
  | var `elem` varSet' =
      let order = fromJust $ elemIndex var varSet'
       in Just (varSet !! order)
  | otherwise = Nothing
toFormalMaybe ('\\' : var : '.' : ' ' : rest) =
  Just "(\\" +++ toFormalMaybe [var] +++ toFormalMaybe rest +++ Just ")"
toFormalMaybe ('\\' : var : ',' : rest) =
  Just "(\\" +++ toFormalMaybe [var] +++ toFormalMaybe ('\\' : rest) +++ Just ")"
toFormalMaybe expr
  | null objects = Just []
  | length objects > 1 = Just "(" +++ toFormalMaybe operator +++ toFormalMaybe operand +++ Just ")"
  | otherwise =
      let object = head objects
          braced = ('(' == head object) && (')' == last object)
          object' = if braced then tail $ init object else object
       in Just "(" +++ toFormalMaybe object' +++ Just ")"
  where
    objects = getCombinedTerms expr
    operator = join $ init objects
    operand = last objects

-- [TODO]
-- fromFormalMaybe :: String -> Maybe String
-- fromFormalMaybe = Just

-- $model
-- ┌───────────────────────────────┐
-- │ Modelling the lambda calculus │
-- └───────────────────────────────┘

type Variable = Int

data Lambda = Var Variable | Abst Variable Lambda | Appl Lambda Lambda deriving (Read, Show, Eq)

i :: Lambda
i = fromJust $ parse "\\x. x"

k :: Lambda
k = fromJust $ parse "\\x,y. x"

k' :: Lambda
k' = fromJust $ parse "\\x,y. y"

s :: Lambda
s = fromJust $ parse "\\x,y,z. xz(yz)"

y :: Lambda
y = fromJust $ parse "\\f. (\\x. f(xx))(\\x. f(xx))"

maybeAbst :: Variable -> Maybe Lambda -> Maybe Lambda
maybeAbst _ Nothing = Nothing
maybeAbst n (Just l) = Just $ Abst n l

maybeAppl :: Maybe Lambda -> Maybe Lambda -> Maybe Lambda
maybeAppl Nothing _ = Nothing
maybeAppl _ Nothing = Nothing
maybeAppl (Just l1) (Just l2) = Just $ Appl l1 l2

parse :: String -> Maybe Lambda
parse [] = Nothing
parse (' ' : rest) = parse rest
parse ('\\' : var : ',' : ' ' : rest) = parse ('\\' : var : ',' : rest)
parse ('\\' : var : '.' : ' ' : ' ' : rest) = parse ('\\' : var : '.' : ' ' : rest)
parse ('I' : rest) = parse $ "(" ++ unparse i ++ ")" ++ rest
parse ('K' : '*' : rest) = parse $ "(" ++ unparse k' ++ ")" ++ rest
parse ('K' : rest) = parse $ "(" ++ unparse k ++ ")" ++ rest
parse ('S' : rest) = parse $ "(" ++ unparse s ++ ")" ++ rest
parse [var]
  | var `elem` varSet' = Just $ Var (fromJust $ elemIndex var varSet')
  | otherwise = Nothing
parse ('\\' : var : rest)
  | var `notElem` varSet' = Nothing
  | head rest == ',' = maybeAbst (fromJust $ elemIndex var varSet') (parse $ '\\' : tail rest)
  | head rest == '.' = maybeAbst (fromJust $ elemIndex var varSet') (parse $ drop 2 rest)
  | otherwise = Nothing
parse str
  | null objects = Nothing
  | length objects == 1 =
      let object = head objects
          braced = ('(' == head object) && (')' == last object)
       in if braced then parse (tail . init $ object) else Nothing
  | otherwise = maybeAppl (parse $ join (init objects)) (parse $ last objects)
  where
    objects = getCombinedTerms str

parseFormal :: String -> Maybe Lambda
parseFormal [] = Nothing
parseFormal str
  | (head str == 'v') && (tail str == replicate n '\'') = Just (Var n)
  where
    n = length str - 1
parseFormal ('(' : '\\' : 'v' : rest) = maybeAbst n (parseFormal $ init $ drop n rest)
  where
    n = prefixLength rest (repeat '\'')
parseFormal ('(' : rest)
  | length objects /= 2 = Nothing
  | otherwise = maybeAppl (parseFormal $ head objects) (parseFormal $ last objects)
  where
    objects = getCombinedTerms $ init rest
parseFormal _ = Nothing

wrapAbst :: Lambda -> String
wrapAbst (Abst n l) = "(" ++ unparse (Abst n l) ++ ")"
wrapAbst l = unparse l

wrapNotVar :: Lambda -> String
wrapNotVar (Var n) = unparse (Var n)
wrapNotVar l = "(" ++ unparse l ++ ")"

unparse :: Lambda -> String
unparse (Var n) = [varSet' !! n]
unparse (Abst n (Abst m l)) = "\\" ++ unparse (Var n) ++ "," ++ (tail . unparse) (Abst m l)
unparse (Abst n l) = "\\" ++ unparse (Var n) ++ ". " ++ wrapAbst l
unparse (Appl l1 l2) = wrapAbst l1 ++ wrapNotVar l2

unparseFormal :: Lambda -> String
unparseFormal (Var n) = varSet !! n
unparseFormal (Abst n l) = "(\\" ++ unparseFormal (Var n) ++ unparseFormal l ++ ")"
unparseFormal (Appl l1 l2) = "(" ++ unparseFormal l1 ++ unparseFormal l2 ++ ")"

-- exercises

toFormalStr :: String -> String
toFormalStr = unparseFormal . fromJust . parse'

toInformalStr :: String -> String
toInformalStr = unparse . fromJust . parseFormal'

-- ┌─────────────────────────────────────┐
-- │ Coding up the logic of lambda terms │
-- └─────────────────────────────────────┘

totalVarSet :: Lambda -> Set Variable
totalVarSet (Var n) = singleton n
totalVarSet (Abst n l) = insert n $ totalVarSet l
totalVarSet (Appl l1 l2) = totalVarSet l1 `union` totalVarSet l2

boundVarSet :: Lambda -> Set Variable
boundVarSet (Var _) = empty
boundVarSet (Abst n l) = insert n $ boundVarSet l
boundVarSet (Appl l1 l2) = boundVarSet l1 `union` boundVarSet l2

freeVarSet :: Lambda -> Set Variable
freeVarSet (Var n) = singleton n
freeVarSet (Abst n l) = delete n $ freeVarSet l
freeVarSet (Appl l1 l2) = freeVarSet l1 `union` freeVarSet l2

isValid :: Lambda -> Bool
isValid (Var _) = True
isValid (Abst n l) = n `notMember` boundVarSet l && isValid l
isValid (Appl l1 l2) =
  isValid l1
    && isValid l2
    && (fv1 `intersection` bv2 == empty)
    && (fv2 `intersection` bv1 == empty)
  where
    fv1 = freeVarSet l1
    bv1 = boundVarSet l1
    fv2 = freeVarSet l2
    bv2 = boundVarSet l2

parse' :: String -> Maybe Lambda
parse' str = parse str >>= checkCondition isValid

parseFormal' :: String -> Maybe Lambda
parseFormal' str = parseFormal str >>= checkCondition isValid

toFormalStr' :: String -> String
toFormalStr' = unparse . fromJust . parse'

toInformalStr' :: String -> String
toInformalStr' = unparse . fromJust . parse'

-- Lambda transformation

substitute :: Lambda -> Variable -> Lambda -> Lambda
substitute (Var n) m l
  | n == m = l
  | otherwise = Var n
substitute (Abst n l1) m l2 = Abst n (substitute l1 m l2)
substitute (Appl l1 l2) m l = Appl (substitute l1 m l) (substitute l2 m l)

reduceSingle :: Lambda -> Lambda
reduceSingle (Appl (Abst n l1) l2) = reduce $ substitute l1 n l2
reduceSingle (Abst n (Appl l (Var m)))
  | n == m && m `notMember` totalVarSet l = reduce l
reduceSingle l = l

reduce :: Lambda -> Lambda
reduce (Var n) = Var n
reduce (Appl l1 l2) = reduceSingle $ Appl (reduce l1) (reduce l2)
reduce (Abst n l) = reduceSingle $ Abst n (reduce l)

-- reduce (Appl (Abst n l1) l2) = reduce $ substitute l1 n l2
-- reduce (Abst n (Appl l (Var m)))
--   | n == m && m `notMember` totalVarSet l = l
-- reduce (Appl (Appl (Abst n l1) l2) l3) = reduce $ Appl (substitute l1 n l2) l3
-- reduce (Appl (Appl l1 l2) l3) = reduce $ Appl (reduce (Appl l1 l2)) l3
-- reduce (Appl l1 l2) = Appl (reduce l1) (reduce l2)
-- reduce (Abst n l) = Abst n (reduce l)
-- reduce l = l

-- Shorthands

substituteStr :: String -> String -> String -> String
substituteStr s1 s2 s3 = unparse $ substitute l1 n l2
  where
    l1 = fromJust $ parse s1
    (Var n) = fromJust $ parse s2
    l2 = fromJust $ parse s3

reduceStr :: String -> String
reduceStr = unparse . reduce . fromJust . parse'
