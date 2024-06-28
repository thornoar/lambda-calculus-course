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
