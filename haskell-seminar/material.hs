-- $intro
-- ┌──────────────┐
-- │ Introduction │
-- └──────────────┘

{-

Haskell по пунктам:

-- Чисто функциональный: нет понятия состояния, 

-}


-- $basic
-- ┌────────────────────┐
-- │ Basic introduction │
-- └────────────────────┘

import Control.Applicative

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibList :: [Int]
fibList = map fib [0..]

square :: Float -> Float
square x = x * x

-- [TODO]
union :: (Eq a) => [a] -> [a] -> [a]
union [] bs = bs
union (a:as) bs
  | a `elem` bs = union as bs
  | otherwise = a : union as bs

-- [TODO]
elemIndex :: (Eq a) => a -> [a] -> Maybe Int
elemIndex a as
  | a `notElem` as = Nothing
  | otherwise = Just $ findIndex a as 0
  where
    findIndex :: (Eq a) => a -> [a] -> Int -> Int
    findIndex a (b:bs) n
      | a == b = n
      | otherwise = findIndex a bs (n+1)

-- [TODO]
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide n m = Just (n `div` m)
