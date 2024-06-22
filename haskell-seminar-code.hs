import Data.List
import Data.Maybe

shiftListLeft :: Int -> [a] -> [a]
shiftListLeft 0 lst = lst
shiftListLeft _ [] = []
shiftListLeft 1 (a:as) = as ++ [a]
shiftListLeft n lst = (shiftListLeft 1) (shiftListLeft (n-1) lst)

shiftListRight :: Int -> [a] -> [a]
shiftListRight n = reverse . (shiftListLeft n) . reverse

varList :: [Char]
varList = shiftListRight 3 ['a'..'z']

(+++) :: Maybe String -> Maybe String -> Maybe String
(+++) s1 s2
    | s1 == Nothing || s2 == Nothing = Nothing
    | otherwise = Just (fromJust s1 ++ fromJust s2)

formalVar :: Char -> Maybe String
formalVar var
    | elem var varList =
        let
            order = fromJust $ elemIndex var varList
            lst = repeat '\''
        in Just ("v" ++ (take order lst))
    | otherwise = Nothing

toFormal :: String -> Maybe String
-- toFormal "x" = "v"
-- toFormal "y" = "v'"
-- toFormal "z" = "v''"
-- toFormal "a" = "v'''"
-- toFormal "b" = "v''''"
-- toFormal "c" = "v'''''"
-- toFormal "f" = "v''''''"
-- toFormal "g" = "v'''''''"
-- toFormal "h" = "v''''''''"
toFormal [] = Just []
toFormal [var] = formalVar var
toFormal ('\\':var:',':rest) = (Just "\\") +++ (toFormal [var]) +++ (Just "(") +++ (toFormal $ '\\':rest) +++ (Just ")")
toFormal ('\\':var:'.':' ':rest) = (Just "\\") +++ (toFormal [var]) +++ (Just "(") +++ (toFormal rest) +++ (Just ")")
toFormal (var1:var2:rest) = (Just "(") +++ (toFormal [var1]) +++ (toFormal [var2]) +++ (Just ")") +++ (toFormal rest)
-- toFormal _ = Nothing

toFormal' :: String -> IO ()
toFormal' = putStrLn . id . fromJust . toFormal
