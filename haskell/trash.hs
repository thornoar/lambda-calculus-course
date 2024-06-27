
-- $conversion
-- ┌─────────────────────────────────────────┐
-- │ Converting formal and informal notation │
-- └─────────────────────────────────────────┘

-- parseFormal' :: String -> Maybe Lambda
-- parseFormal' str = parseFormal (preprocess str) >>= checkCondition isValid

-- _toInformal' :: String -> IO ()
-- _toInformal' = putStrLn . unparse . fromJust . parseFormal'

-- parseFormal :: String -> Maybe Lambda
-- parseFormal [] = Nothing
-- parseFormal str
--   | (head str == 'v') && (tail str == replicate n '\'') = Just (Var n)
--   where
--     n = length str - 1
-- parseFormal ('(' : '\\' : 'v' : rest) = maybeAbst n (parseFormal $ init $ drop n rest)
--   where
--     n = prefixLength rest (repeat '\'')
-- parseFormal ('(' : rest)
--   | length objects /= 2 = Nothing
--   | otherwise = maybeAppl (parseFormal $ head objects) (parseFormal $ last objects)
--   where
--     objects = getCombinedTerms $ init rest
-- parseFormal _ = Nothing

-- (&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
-- (&&&) f g x = f x && g x

-- (...) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
-- (...) g f x = f x >>= g
--
-- infixr 9 ...

-- preprocess ('\\':var:',':' ':rest) = preprocess $ '\\' : var : ',' : rest
-- preprocess ('\\':var:'.':' ':' ': rest) = preprocess $ '\\' : var : '.' : ' ' : rest
-- preprocess ('\\':var:'.':' ': rest) = '\\' : var : '.' : ' ' : preprocess rest

-- parse (' ' : rest) = parse rest
-- parse ('\\' : var : ',' : ' ' : rest) = parse ('\\' : var : ',' : rest)
-- parse ('\\' : var : '.' : ' ' : rest) = parse ('\\' : var : '.' : rest)

-- maybeAbst :: Variable -> Maybe Lambda -> Maybe Lambda
-- maybeAbst _ Nothing = Nothing
-- maybeAbst n (Just l) = Just $ Abst n l
--
-- maybeAppl :: Maybe Lambda -> Maybe Lambda -> Maybe Lambda
-- maybeAppl Nothing _ = Nothing
-- maybeAppl _ Nothing = Nothing
-- maybeAppl (Just l1) (Just l2) = Just $ Appl l1 l2

-- toFormal :: String -> String
-- toFormal [] = []
-- toFormal [var]
--   | var `elem` varSet' =
--       let order = fromJust $ elemIndex var varSet'
--        in varSet !! order
--   | otherwise = [var]
-- toFormal ('\\' : var : '.' : ' ' : rest) = "(\\" ++ toFormal [var] ++ toFormal rest ++ ")"
-- toFormal ('\\' : var : ',' : rest) = "(\\" ++ toFormal [var] ++ toFormal ('\\' : rest) ++ ")"
-- toFormal expr
--   | null objects = []
--   | length objects == 1 =
--       let object = head objects
--           braced = ('(' == head object) && (')' == last object)
--           object' = if braced then (tail . init) object else object
--        in toFormal object'
--   | otherwise =
--       let operator = join $ init objects
--           operand = last objects
--        in "(" ++ toFormal operator ++ toFormal operand ++ ")"
--   where
--     objects = getCombinedTerms expr

-- [TODO]
-- fromFormal :: String -> String
-- fromFormal = id

-- $maybe
-- ┌─────────────────────────────────────┐
-- │ Adding failing conditions via Maybe │
-- └─────────────────────────────────────┘

-- io :: Maybe String -> IO ()
-- io Nothing = putStrLn "error: invalid input"
-- io (Just str) = putStrLn str

-- (+++) :: (Eq a) => Maybe [a] -> Maybe [a] -> Maybe [a]
-- (+++) s1 s2
--   | isNothing s1 || isNothing s2 = Nothing
--   | otherwise = Just (fromJust s1 ++ fromJust s2)

-- toFormalMaybe :: String -> Maybe String
-- toFormalMaybe [] = Nothing
-- toFormalMaybe [var]
--   | var `elem` varSet' =
--       let order = fromJust $ elemIndex var varSet'
--        in Just (varSet !! order)
--   | otherwise = Nothing
-- toFormalMaybe ('\\' : var : '.' : ' ' : rest) =
--   Just "(\\" +++ toFormalMaybe [var] +++ toFormalMaybe rest +++ Just ")"
-- toFormalMaybe ('\\' : var : ',' : rest) =
--   Just "(\\" +++ toFormalMaybe [var] +++ toFormalMaybe ('\\' : rest) +++ Just ")"
-- toFormalMaybe expr
--   | null objects = Just []
--   | length objects > 1 = Just "(" +++ toFormalMaybe operator +++ toFormalMaybe operand +++ Just ")"
--   | otherwise =
--       let object = head objects
--           braced = ('(' == head object) && (')' == last object)
--           object' = if braced then tail $ init object else object
--        in Just "(" +++ toFormalMaybe object' +++ Just ")"
--   where
--     objects = getCombinedTerms expr
--     operator = join $ init objects
--     operand = last objects

-- [TODO]
-- fromFormalMaybe :: String -> Maybe String
-- fromFormalMaybe = Just
