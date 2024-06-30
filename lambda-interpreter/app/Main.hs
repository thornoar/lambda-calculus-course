module Main where

import Lambda
import System.Console.Haskeline
import Data.Map (Map, empty, insert, member, (!))
import Text.Read (readMaybe)
-- import Data.List (replicate)

-- ┌─────────────────────┐
-- │ Command definitions │
-- └─────────────────────┘

data Mode = SILENT | REPEAT | PRINT | REDUCE | STEPS | CONGR | EQUIV | SHOW | READ | TOFORMAL | TOINFORMAL | FORMAT
  deriving (Read, Show)

mapFromList :: (Ord a) => [(a, a, b, a)] -> Map a b
mapFromList [] = empty
mapFromList ((a1,a2,b,_):rest) =
  insert a1 b $
  insert a2 b $
  mapFromList rest

commandList :: [(String, String, Mode, String)]
commandList = [
    ("s", "silent", SILENT, "swallows input and returns it"),
    ("rp", "repeat", REPEAT, "repeats input as-is"),
    ("pr", "print", PRINT, "parses a given lambda expression and prints it"),
    ("r", "reduce", REDUCE, "reduces a given lambda expression and prints it"),
    ("rs", "steps", STEPS, "enters a step-by-step reduction process"),
    ("cg", "conrg", CONGR, "prints whether two given expressions are congruent (with variable replacement)"),
    ("eq", "equiv", EQUIV, "prints whether two given expressions are reducible to the same one"),
    ("sh", "show", SHOW, "prints the internal representation of a lambda expression"),
    ("rd", "read", READ, "evaluates a given internal representation and prints it"),
    ("tf", "toformal", TOFORMAL, "converts to the formal notation"),
    ("ti", "toinformal", TOINFORMAL, "converts to the informal notation"),
    ("s", "format", FORMAT, "adjusts the bound variables to avoid collisions")
  ]

commandMap :: Map String Mode
commandMap = mapFromList commandList

-- ┌──────────────────────────┐
-- │ Output string processing │
-- └──────────────────────────┘

splitByFirstSpace :: String -> Maybe Int
splitByFirstSpace [] = Nothing
splitByFirstSpace (' ':_) = Just 0
splitByFirstSpace (_:rest) = (+ 1) <$> splitByFirstSpace rest

color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m"

errorString :: String -> String
errorString str = color "1;31" "error: " ++ str ++ "."

adjustSpaces :: Int -> String -> String
adjustSpaces n str
  | len > n = str
  | otherwise = str ++ replicate (n - len) ' '
  where
    len = length str

help :: InputT IO ()
help = do
  let
    getCommandInfo :: (String, String, Mode, String) -> InputT IO ()
    getCommandInfo (k1,k2,m,descr) = outputStrLn $
      adjustSpaces 7 ("  *" ++ k1 ++ ",") ++
      adjustSpaces 14 ("*" ++ k2) ++
      adjustSpaces 22 ("the " ++ show m ++ " mode ") ++
      descr ++ "."
  outputStrLn "usage: [>command | <command | @command] [LAMBDA]"
  outputStrLn "commands:"
  mapM_ getCommandInfo commandList

-- ┌─────────────────────────────────┐
-- │ Evaluating and returning output │
-- └─────────────────────────────────┘

printGeneral :: (String -> InputT IO ()) -> Maybe String -> InputT IO ()
printGeneral f Nothing = f $ errorString "Incorect syntax"
printGeneral f (Just str) = f . ("| " ++) . color "34" $ str

printLn :: Maybe String -> InputT IO ()
printLn = printGeneral outputStrLn

printReturn :: (String -> Maybe String) -> String -> InputT IO (Maybe String)
printReturn f str = do
  let res = f str
  printLn res
  return res

withTwo :: (Lambda -> Lambda -> Bool) -> String -> InputT IO (Maybe String)
withTwo f str1 = do
  let ml1 = parse' str1
      print' :: Maybe Bool -> InputT IO (Maybe String)
      print' mb = do
        let res = show <$> mb
        printLn res
        return res
  str2 <- getInputLine $ "  (" ++ color "33" "AND" ++ "): "
  case str2 of
    Nothing -> return Nothing
    Just [] -> return Nothing
    _ -> do
      let ml2 = str2 >>= parse'
      print' $ raise f ml1 ml2

evalOnce :: Mode -> String -> InputT IO (Maybe String)
evalOnce SILENT = return . Just
evalOnce REPEAT = printReturn Just
evalOnce PRINT = printReturn (fmap unparse' . parse')
evalOnce REDUCE = printReturn (fmap (unparse' . reduce) . parse')
evalOnce STEPS = print' . parse'
  where
    print' :: Maybe Lambda -> InputT IO (Maybe String)
    print' Nothing = do
      printLn Nothing
      return Nothing
    print' (Just l) = showSteps l
    showSteps :: Lambda -> InputT IO (Maybe String)
    showSteps l = do
      let lstr = Just $ unparse' l
      printGeneral outputStr lstr
      input <- getInputLine ""
      case input of
        Nothing -> return lstr
        _ -> do
          let (l', found) = reduceStep l
          if found
          then showSteps l'
          else return lstr
evalOnce CONGR = withTwo equiv
evalOnce EQUIV = withTwo equiv'
evalOnce SHOW = printReturn (fmap show . parse')
evalOnce READ = printReturn (fmap unparse' . readMaybe)
evalOnce TOFORMAL = printReturn (fmap unparseFormal . parse')
evalOnce TOINFORMAL = printReturn (fmap unparse' . parse')
evalOnce FORMAT = printReturn (fmap (unparse . adjustBoundVars) . parse')

-- ┌───────────────────┐
-- │ Building the REPL │
-- └───────────────────┘

handleCommand :: String -> (Mode -> String -> InputT IO (Maybe String)) -> InputT IO (Maybe String)
handleCommand str f = do
  let mn = splitByFirstSpace str
      err :: String -> InputT IO (Maybe String)
      err str' = do
        outputStrLn $ errorString str'
        return Nothing
  case mn of
    Nothing -> err "Could not parse command and argument"
    (Just n) -> do
      let cmd = take n str
          str' = drop (n+1) str
      if member cmd commandMap
      then
        if null str'
        then err "No argument given"
        else f (commandMap ! cmd) str'
      else err "Unknown command"

pipe :: Mode -> Mode -> String -> InputT IO (Maybe String)
pipe basemode curmode str = do
  val <- eval curmode str
  case val of
    Nothing -> return $ Just "a-ha!"
    (Just val') -> evalOnce basemode val'

eval :: Mode -> String -> InputT IO (Maybe String)
eval mode str = case str of
  [] -> return Nothing
  ('<':rest) -> handleCommand rest (pipe mode)
  ('@':rest) -> handleCommand rest eval
  input -> evalOnce mode input

loop :: Mode -> InputT IO ()
loop mode = do
  minput <- getInputLine $ "(" ++ color "33" (show mode) ++ "): "
  case minput of
    Nothing -> return ()
    Just [] -> loop mode
    Just "help" -> help >> loop mode
    Just "quit" -> return ()
    Just "q" -> return ()
    Just ('>':rest) ->
      if member rest commandMap
      then loop (commandMap ! rest)
      else do
        outputStrLn $ errorString "Unknown command"
        loop mode
    Just input -> do
      _ <- eval mode input
      loop mode

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just ".lambda-interpreter-history",
  autoAddHistory = True
}

main :: IO ()
main = runInputT settings
  $ loop PRINT
