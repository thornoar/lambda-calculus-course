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

square :: Float -> Float
square x = x*x


