module Main where

import Lambda
import System.Console.Haskeline

data Mode = PRINT | REDUCE | STEPS | EQUIV | SHOW deriving (Read, Show)

color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m"

io :: (Lambda -> String) -> Maybe Lambda -> InputT IO ()
io _ Nothing = outputStrLn $ color "1;31" "error:" ++ " incorrect syntax."
io f (Just l) = outputStrLn $ f l

help :: InputT IO ()
help = do
  outputStrLn "usage: [:rp | :rs | :r | :p | :s] [LAMBDA]"

eval :: Mode -> String -> InputT IO ()
eval PRINT = io (("| " ++) . color "34" . unparse') . parse'
eval REDUCE = io (("| " ++) . color "34" . unparse' . reduce) . parse'
eval STEPS = io' . parse'
  where
    io' :: Maybe Lambda -> InputT IO ()
    io' Nothing = outputStrLn $ color "1;31" "error:" ++ " Incorrect syntax."
    io' (Just l) = showSteps l
    showSteps :: Lambda -> InputT IO ()
    showSteps l
      | found = do
        outputStr $ "| " ++ color "34" (unparse' l')
        _ <- getInputLine ""
        showSteps l'
      | otherwise = return ()
      where
        (l', found) = reduceStep l
eval EQUIV = showEquiv
  where
    io' :: Maybe Bool -> InputT IO ()
    io' Nothing = outputStrLn $ color "1;31" "error:" ++ " Incorrect syntax."
    io' (Just b) = outputStrLn . ("| " ++) . color "33" . show $ b
    showEquiv :: String -> InputT IO ()
    showEquiv str1 = do
      let ml1 = parse' str1
      str2 <- getInputLine $ "  (" ++ color "37" "AND" ++ ") > "
      let ml2 = str2 >>= parse'
      io' $ raise equiv' ml1 ml2
eval SHOW = io (("| " ++) . color "35" . show) . parse'

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just "./histfile",
  autoAddHistory = True
}

main :: IO ()
main = runInputT settings $ loop REDUCE
  where
    loop :: Mode -> InputT IO ()
    loop mode = do
      minput <- getInputLine $ "(" ++ color "37" (show mode) ++ ") > "-- ++ color "37" ">" ++ " "
      case minput of
        Nothing -> return ()
        Just "" -> loop mode
        Just ":quit" -> return ()
        Just ":q" -> return ()
        Just ":help" -> help >> loop mode
        Just ":h" -> help >> loop mode
        Just ":steps" -> loop STEPS
        Just ":reduce" -> loop REDUCE
        Just ":print" -> loop PRINT
        Just ":equiv" -> loop EQUIV
        Just ":show" -> loop SHOW
        Just ":rs" -> loop STEPS
        Just ":r" -> loop REDUCE
        Just ":p" -> loop PRINT
        Just ":eq" -> loop EQUIV
        Just ":s" -> loop SHOW
        Just (':':'r':'s':' ':rest) -> eval STEPS rest >> loop mode
        Just (':':'r':' ':rest) -> eval REDUCE rest >> loop mode
        Just (':':'p':' ':rest) -> eval PRINT rest >> loop mode
        Just (':':'e':'q':' ':rest) -> eval EQUIV rest >> loop mode
        Just (':':'s':' ':rest) -> eval SHOW rest >> loop mode
        Just input -> eval mode input >> loop mode
