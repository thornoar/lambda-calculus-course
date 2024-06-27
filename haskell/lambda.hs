import Control.Monad
import Data.List (elemIndex, nub)
import Data.Maybe
import Data.Char (isAlpha)
import Data.Set (Set, delete, empty, insert, intersection, member, notMember, singleton, union, toList)

prefixLength :: (Eq a) => [a] -> [a] -> Int
prefixLength [] _ = 0
prefixLength _ [] = 0
prefixLength (x : xs) (y : ys)
  | x == y = 1 + prefixLength xs ys
  | otherwise = 0

checkCondition :: (a -> Bool) -> a -> Maybe a
checkCondition f a = if f a then Just a else Nothing

getCombinedTerms :: String -> [String]
getCombinedTerms [] = []
getCombinedTerms (' ' : rest) = getCombinedTerms rest
getCombinedTerms ('(' : ')' : rest) = getCombinedTerms rest
getCombinedTerms (var : rest)
  | null rest = [[var]]
  | isAlpha var = (var : take n1 rest) : getCombinedTerms (drop n1 rest)
  | var == '(' = (var : take n2 rest) : getCombinedTerms (drop n2 rest)
  | otherwise = [var] : getCombinedTerms rest
  where
    findAlpha :: String -> Int -> Int
    findAlpha [] m = m
    findAlpha (a:as) m
      | a == '(' = m
      | isAlpha a = m
      | otherwise = findAlpha as (m+1)
    n1 :: Int
    n1 = findAlpha rest 0
    findClosingBracket :: String -> Int -> Int -> Int
    findClosingBracket [] _ _ = 0
    findClosingBracket ('(' : rest') step count = findClosingBracket rest' (step + 1) (count + 1)
    findClosingBracket (')' : rest') step count
      | step == 0 = count+1
      | otherwise = findClosingBracket rest' (step - 1) (count + 1)
    findClosingBracket (a : rest') step count = findClosingBracket rest' step (count + 1)
    n2 :: Int
    n2 = findClosingBracket rest 0 0

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
varSet' = ['x', 'y', 'z', 'w', 'u', 't', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'p', 'q']

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
parse ('Y' : rest) = parse $ "(" ++ unparse y ++ ")" ++ rest
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

-- [TODO]
-- toFormalIO :: String -> String
-- toFormalIO = unparseFormal . fromJust . parse
--
-- toInformalIO :: String -> String
-- toInformalIO = unparse . fromJust . parseFormal

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

-- Lambda transformation

substitute :: Lambda -> Variable -> Lambda -> Lambda
substitute (Var n) m l
  | n == m = l
  | otherwise = Var n
substitute (Abst n l1) m l2
  | n `notElem` bnd2 = Abst n (substitute l1 m l2)
  | otherwise = Abst n (substitute l1 m l2')
  where
    bnd2 = toList $ boundVarSet l2
    l2' = changeBoundVar l2 n (1 + maximum bnd2)
substitute (Appl l1 l2) m l = Appl (substitute l1 m l) (substitute l2 m l)

changeBoundVar :: Lambda -> Variable -> Variable -> Lambda
changeBoundVar (Var n) _ _ = Var n
changeBoundVar (Abst n l) m1 m2
  | n == m1 = Abst m2 $ substitute l m1 (Var m2)
  | otherwise = Abst n $ changeBoundVar l m1 m2
changeBoundVar (Appl l1 l2) m1 m2 = Appl (changeBoundVar l1 m1 m2) (changeBoundVar l2 m1 m2)

reduceStep :: Lambda -> (Lambda, Bool)
reduceStep (Appl (Abst n l1) l2) = (substitute l1 n l2, True)
reduceStep (Abst n (Appl l (Var m)))
  | n == m && m `notMember` freeVarSet l = (l, True)
reduceStep (Var n) = (Var n, False)
reduceStep (Appl l1 l2)
  | found1 = (Appl l1' l2, True)
  | found2 = (Appl l1 l2', True)
  | otherwise = (Appl l1 l2, False)
  where
    (l1', found1) = reduceStep l1
    (l2', found2) = reduceStep l2
reduceStep (Abst n l) = (Abst n l', found)
  where
    (l', found) = reduceStep l

reduce :: Lambda -> Lambda
reduce l
  | found = reduce l'
  | otherwise = l
  where 
    (l', found) = reduceStep l

reduceTimes :: Int -> Lambda -> Lambda
reduceTimes 0 l = l
reduceTimes n l
  | found = reduceTimes (n-1) l'
  | otherwise = l
  where 
    (l', found) = reduceStep l

equiv :: Lambda -> Lambda -> Bool
equiv (Var n1) (Var n2)
  | n1 == n2 = True
  | otherwise = False
equiv (Appl p1 p2) (Appl q1 q2) = equiv p1 q1 && equiv p2 q2
equiv (Abst n1 l1) (Abst n2 l2)
  | n1 == n2 = equiv l1 l2
  | otherwise = equiv l1 (substitute l2 n2 (Var n1))
equiv _ _ = False

equiv' :: Lambda -> Lambda -> Bool
equiv' l1 l2 = equiv (reduce l1) (reduce l2)

-- Shorthands

lm :: String -> Lambda
lm = fromJust . parse'

toFormalIO' :: String -> IO ()
toFormalIO' = putStrLn . unparseFormal . fromJust . parse'

toInformalIO' :: String -> IO ()
toInformalIO' = putStrLn . unparse . fromJust . parseFormal'

printIO :: String -> IO ()
printIO = putStrLn . unparse . fromJust . parse'

substituteIO :: String -> String -> String -> IO ()
substituteIO s1 s2 s3 = putStrLn $ unparse $ substitute l1 n l2
  where
    l1 = fromJust $ parse s1
    (Var n) = fromJust $ parse s2
    l2 = fromJust $ parse s3

reduceIO :: String -> IO ()
reduceIO = putStrLn . unparse . reduce . fromJust . parse'

reduceTimesIO :: Int -> String -> IO ()
reduceTimesIO n = putStrLn . unparse . reduceTimes n . fromJust . parse'

equivIO :: String -> String -> IO ()
equivIO s1 s2 = print $ equiv (fromJust . parse' $ s1) (fromJust . parse' $ s2)

equivIO' :: String -> String -> IO ()
equivIO' s1 s2 = print $ equiv' (fromJust . parse' $ s1) (fromJust . parse' $ s2)
