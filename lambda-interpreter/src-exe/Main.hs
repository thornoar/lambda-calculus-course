module Main where

import Lambda
import System.Console.Haskeline

data Mode = REPEAT | REDUCE | REDUCESTEPS | PRINT | SHOW deriving (Read, Show)

io :: (Lambda -> String) -> Maybe Lambda -> InputT IO ()
io _ Nothing = outputStrLn "error: incorrect syntax."
io f (Just l) = outputStrLn $ f l

help :: InputT IO ()
help = do
  outputStrLn "cannot help you"

eval :: Mode -> String -> InputT IO ()
eval REPEAT = outputStrLn
eval REDUCE = io (unparse' . reduce) . parse'
eval REDUCESTEPS = io (unparse' . reduceTimes 1) . parse'
eval PRINT = io unparse' . parse'
eval SHOW = io show . parse'

main :: IO ()
main = runInputT defaultSettings $ loop REPEAT
  where
    loop :: Mode -> InputT IO ()
    loop mode = do
      minput <- getInputLine $ "> " ++ show mode ++ ": "
      case minput of
        Nothing -> return ()
        Just "" -> loop mode
        Just ":quit" -> return ()
        Just ":q" -> return ()
        Just ":help" -> help >> loop mode
        Just ":h" -> help >> loop mode
        Just ":repeat" -> loop REPEAT
        Just ":reduce" -> loop REDUCE
        Just ":steps" -> loop REDUCESTEPS
        Just ":print" -> loop PRINT
        Just ":show" -> loop SHOW
        Just (':':'r':'p':' ':rest) -> eval REPEAT rest >> loop mode
        Just (':':'r':'s':' ':rest) -> eval REDUCESTEPS rest >> loop mode
        Just (':':'r':' ':rest) -> eval REDUCE rest >> loop mode
        Just (':':'p':' ':rest) -> eval PRINT rest >> loop mode
        Just (':':'s':' ':rest) -> eval SHOW rest >> loop mode
        Just input -> eval mode input >> loop mode
