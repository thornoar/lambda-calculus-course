module Main where

import Lambda
import System.Console.Haskeline

data Mode = REPEAT | REDUCE | STEPS | PRINT | SHOW deriving (Read, Show)

color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m"

io :: (Lambda -> String) -> Maybe Lambda -> InputT IO ()
io _ Nothing = outputStrLn $ (color "1;31" "error:") ++ " incorrect syntax."
io f (Just l) = outputStrLn $ f l

help :: InputT IO ()
help = do
  outputStrLn "usage: [:rp | :rs | :r | :p | :s] [LAMBDA]"

eval :: Mode -> String -> InputT IO ()
eval REPEAT = outputStrLn . ("Input was: " ++)
eval REDUCE = io (("> " ++) . color "34" . unparse' . reduce) . parse'
eval STEPS = io' . parse'
  where
    io' :: Maybe Lambda -> InputT IO ()
    io' Nothing = outputStrLn $ (color "1;31" "error:") ++ " Incorrect syntax."
    io' (Just l) = showSteps l
    showSteps :: Lambda -> InputT IO ()
    showSteps l
      | found = do
        outputStr $ "> " ++ color "34" (unparse' l')
        _ <- getInputLine ""
        showSteps l'
      | otherwise = return ()
      where
        (l', found) = reduceStep l
eval PRINT = io (("> " ++) . color "33" . unparse') . parse'
eval SHOW = io (("> " ++) . color "35" . show) . parse'

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just "./histfile",
  autoAddHistory = True
}

main :: IO ()
main = runInputT settings $ loop REPEAT
  where
    loop :: Mode -> InputT IO ()
    loop mode = do
      minput <- getInputLine $ "(\ESC[37m" ++ show mode ++ "\ESC[0m): "
      case minput of
        Nothing -> return ()
        Just "" -> loop mode
        Just ":quit" -> return ()
        Just ":q" -> return ()
        Just ":help" -> help >> loop mode
        Just ":h" -> help >> loop mode
        Just ":repeat" -> loop REPEAT
        Just ":steps" -> loop STEPS
        Just ":reduce" -> loop REDUCE
        Just ":print" -> loop PRINT
        Just ":show" -> loop SHOW
        Just ":rp" -> loop REPEAT
        Just ":rs" -> loop STEPS
        Just ":r" -> loop REDUCE
        Just ":p" -> loop PRINT
        Just ":s" -> loop SHOW
        Just (':':'r':'p':' ':rest) -> eval REPEAT rest >> loop mode
        Just (':':'r':'s':' ':rest) -> eval STEPS rest >> loop mode
        Just (':':'r':' ':rest) -> eval REDUCE rest >> loop mode
        Just (':':'p':' ':rest) -> eval PRINT rest >> loop mode
        Just (':':'s':' ':rest) -> eval SHOW rest >> loop mode
        Just input -> eval mode input >> loop mode
