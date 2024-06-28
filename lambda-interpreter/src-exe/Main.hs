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

eval_ :: Mode -> String -> InputT IO ()
eval_ REPEAT = outputStrLn
eval_ REDUCE = io (unparse' . reduce) . parse'
eval_ REDUCESTEPS = io (unparse' . reduceTimes 1) . parse'
eval_ PRINT = io unparse' . parse'
eval_ SHOW = io show . parse'

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
        Just (':':'r':'p':' ':rest) -> eval_ REPEAT rest >> loop mode
        Just (':':'r':'s':' ':rest) -> eval_ REDUCESTEPS rest >> loop mode
        Just (':':'p':' ':rest) -> eval_ PRINT rest >> loop mode
        Just (':':'s':' ':rest) -> eval_ SHOW rest >> loop mode
        Just input -> eval_ mode input >> loop mode
