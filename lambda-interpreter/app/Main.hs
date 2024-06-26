module Main where

import Lambda
import System.Console.Haskeline
import System.Environment (getArgs)
import Data.Map (Map, empty, insert, member, notMember, (!))
import Text.Read (readMaybe)
import Data.Maybe (isNothing)
import Control.Monad (when)

-- ┌─────────────────────┐
-- │ Command definitions │
-- └─────────────────────┘

data Mode =
  RETURN | REPEAT | PRINT | REDUCE | SUBS | STEPS | CONGR | EQUIV | SHOW | READ | TOFORMAL | TOINFORMAL | FORMAT
  deriving (Read, Show)

mapFromList :: (Ord a) => [(a, a, b, a)] -> Map a b
mapFromList [] = empty
mapFromList ((a1,a2,b,_):rest) =
  insert a1 b $
  insert a2 b $
  mapFromList rest

commandList :: [(String, String, Mode, String)]
commandList = [
    ("rt", "return", RETURN, "swallows input and returns it"),
    ("rp", "repeat", REPEAT, "repeats input as-is"),
    ("pr", "print", PRINT, "parses a given lambda expression and prints it"),
    ("r", "reduce", REDUCE, "reduces a given lambda expression and prints it"),
    ("sb", "subs", SUBS, "substitutes a given expression in place of a given variable"),
    ("rs", "steps", STEPS, "enters a step-by-step reduction process"),
    ("cr", "congr", CONGR, "prints whether two given expressions are congruent (with variable replacement)"),
    ("eq", "equiv", EQUIV, "prints whether two given expressions are reducible to the same one"),
    ("sh", "show", SHOW, "prints the internal representation of a lambda expression"),
    ("rd", "read", READ, "evaluates a given internal representation and prints it"),
    ("tf", "toformal", TOFORMAL, "converts to the formal notation"),
    ("ti", "toinformal", TOINFORMAL, "converts to the informal notation"),
    ("fm", "format", FORMAT, "adjusts the bound variables to avoid collisions")
  ]

commandMap :: Map String Mode
commandMap = mapFromList commandList

-- ┌────────────────────────────────┐
-- │ Input/Output string processing │
-- └────────────────────────────────┘

(>>==) :: (Monad m) => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
(>>==) Nothing _ = return Nothing
(>>==) (Just a) f = f a

splitByFirstSpace :: String -> Maybe Int
splitByFirstSpace [] = Nothing
splitByFirstSpace (' ':_) = Just 0
splitByFirstSpace (_:rest) = (+ 1) <$> splitByFirstSpace rest

color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m"

calibrateLengthPost :: Int -> Char -> String -> String
calibrateLengthPost n rep str
  | len > n = str
  | otherwise = str ++ replicate (n - len) rep
  where
    len = length str

calibrateLengthPre :: Int -> Char -> String -> String
calibrateLengthPre n rep str
  | len > n = str
  | otherwise = replicate (n - len) rep ++ str
  where
    len = length str

promptLength :: Int
promptLength = 15

prompt :: Char -> String -> String
prompt rep body = "\ESC[0m(" ++ color "33" body ++ ") " ++ replicate n rep ++ ": \ESC[34m"
  where n = promptLength - length body - 5

getColoredInputLine :: String -> InputT IO (Maybe String)
getColoredInputLine pref = do
  res <- getInputLine pref
  outputStr "\ESC[0m"
  return res

-- ┌─────────────────────────────────┐
-- │ Evaluating and returning output │
-- └─────────────────────────────────┘

printGeneral :: (String -> InputT IO ()) -> Maybe String -> InputT IO (Maybe String)
printGeneral _ Nothing = return Nothing
printGeneral f (Just str) = do
  f . ("| " ++) . color "34" $ str
  return $ Just str

printLn :: Maybe String -> InputT IO (Maybe String)
printLn = printGeneral outputStrLn

withTwo ::
  (a -> b -> c) ->
  (String -> Maybe a) ->
  (String -> Maybe b) ->
  (c -> String) ->
  String ->
  String ->
  InputT IO (Maybe String)
withTwo f rA rB sC prt str1 = rA str1 >>== \a -> do
  minput <- getColoredInputLine $ prompt ' ' prt
  mstr2 <- minput >>== eval RETURN
  let mb = mstr2 >>= rB
  mb >>== \b -> printLn $ Just $ sC $ f a b

withThree ::
  (a -> b -> c -> d) ->
  (String -> Maybe a) ->
  (String -> Maybe b) ->
  (String -> Maybe c) ->
  (d -> String) ->
  (String, String) ->
  String ->
  InputT IO (Maybe String)
withThree f rA rB rC sD (prt1, prt2) str1 = rA str1 >>== \a -> do
  minput2 <- getColoredInputLine $ prompt ' ' prt1
  mstr2 <- minput2 >>== eval RETURN
  let mb = mstr2 >>= rB
  mb >>== \b -> do
    minput3 <- getColoredInputLine $ prompt ' ' prt2
    mstr3 <- minput3 >>== eval RETURN
    let mc = mstr3 >>= rC
    mc >>== \c -> printLn $ Just $ sD $ f a b c

evalOnce :: Mode -> String -> InputT IO (Maybe String)
evalOnce RETURN = return . Just
evalOnce REPEAT = printLn . Just
evalOnce PRINT = printLn . fmap unparse' . parse'
evalOnce REDUCE = printLn . fmap (unparse' . reduce) . parse'
evalOnce SUBS = withThree substitute parse' getVar parse' unparse' ("VAR", "EXPR")
  where
    getVar :: String -> Maybe Int
    getVar str = case mexpr of
      Just (Var n) -> Just n
      _ -> Nothing
      where mexpr = parse' str
evalOnce STEPS = print' . parse'
  where
    print' :: Maybe Lambda -> InputT IO (Maybe String)
    print' Nothing = return Nothing
    print' (Just l) = showSteps l
    showSteps :: Lambda -> InputT IO (Maybe String)
    showSteps l = do
      let lstr = Just $ unparse' l
      _ <- printGeneral outputStr lstr
      input <- getColoredInputLine ""
      case input of
        Nothing -> return lstr
        _ -> do
          let (l', found) = reduceStep l
          if found
          then showSteps l'
          else return lstr
evalOnce CONGR = withTwo equiv parse' parse' show "AND"
evalOnce EQUIV = withTwo equiv' parse' parse' show "AND"
evalOnce SHOW = printLn . fmap show . parse'
evalOnce READ = printLn . fmap unparse' . readMaybe
evalOnce TOFORMAL = printLn . fmap unparseFormal . parse'
evalOnce TOINFORMAL = printLn . fmap unparse' . parse'
evalOnce FORMAT = printLn . fmap (unparse . adjustBoundVars) . parse'

-- ┌────────────────┐
-- │ Error handling │
-- └────────────────┘

errorString :: String -> String
errorString str = color "1;31" "error: " ++ str ++ "."

printError :: String -> InputT IO ()
printError str' = outputStrLn $ errorString str'

printInputError :: InputT IO ()
printInputError = printError "Incorrect input"

-- ┌──────────────────────────────────┐
-- │ Command piping and mode changing │
-- └──────────────────────────────────┘

handleCommand :: String -> (Mode -> String -> InputT IO (Maybe String)) -> InputT IO (Maybe String)
handleCommand str f = do
  let mn = splitByFirstSpace str
  mn >>== \n -> do
    let cmd = take n str
        str' = drop (n+1) str
    if notMember cmd commandMap || null str'
    then return Nothing
    else f (commandMap ! cmd) str'

pipe :: Mode -> Mode -> String -> InputT IO (Maybe String)
pipe basemode curmode str = do
  val <- eval curmode str
  val >>== evalOnce basemode

eval :: Mode -> String -> InputT IO (Maybe String)
eval mode str = case str of
  [] -> return Nothing
  (' ':rest) -> eval mode rest
  ('<':rest) -> handleCommand rest (pipe mode)
  ('@':rest) -> handleCommand rest eval
  input -> evalOnce mode input

loop :: Mode -> InputT IO ()
loop mode = do
  minput <- getColoredInputLine $ prompt '#' (show mode)
  case minput of
    Nothing -> return ()
    Just [] -> loop mode
    Just "help" -> helpAction >> loop mode
    Just "quit" -> return ()
    Just "q" -> return ()
    Just ('>':rest) ->
      if member rest commandMap
      then loop (commandMap ! rest)
      else do
        printError "Unknown command"
        loop mode
    Just input -> do
      val <- eval mode input
      when (isNothing val) $ printError "Exception while processing input"
      loop mode

-- ┌───────────────────┐
-- │ Building the REPL │
-- └───────────────────┘

data Options = Options {
    helpOpt :: Bool,
    modeOpt :: Mode,
    errorOpt :: Bool
  }

parseOptions :: [String] -> Options
parseOptions [] = Options { helpOpt = False, modeOpt = REPEAT, errorOpt = False }
parseOptions (opt:rest)
  | opt == "-h" || opt == "--help" = (parseOptions rest) { helpOpt = True }
  | (opt == "-m" || opt == "--mode") && not (null rest) && member mode commandMap =
    (parseOptions (tail rest)) { modeOpt = commandMap ! mode }
  | otherwise = Options { errorOpt = True, helpOpt = False, modeOpt = REPEAT }
  where
    mode = head rest

helpAction :: InputT IO ()
helpAction = do
  let
    l1 = 7
    l2 = 14
    l3 = 22
    getCommandInfo :: (String, String, Mode, String) -> InputT IO ()
    getCommandInfo (k1,k2,m,desc) = outputStrLn $
      calibrateLengthPost l1 ' ' ("  *" ++ k1 ++ ",") ++
      calibrateLengthPost l2 ' ' ("*" ++ k2) ++
      calibrateLengthPost l3 ' ' ("the " ++ show m ++ " mode ") ++
      desc ++ "."
    getFlagInfo :: (String, String, String) -> InputT IO ()
    getFlagInfo (s,l,desc) = outputStrLn $ 
      calibrateLengthPost l1 ' ' s ++
      calibrateLengthPost (l2+l3) ' ' l ++
      desc ++ "."
  outputStrLn "\nlambda-interpreter --- a command line utility for parsing and processing lambda terms."
  outputStrLn "\nflags:"
  getFlagInfo ("-h", "--help", "helps")
  getFlagInfo ("-m", "--mode", "sets the initial mode")
  outputStrLn "\nREPL usage: [>command | <command | @command] [LAMBDA]"
  outputStrLn "commands:"
  mapM_ getCommandInfo commandList

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just ".lambda-interpreter-history",
  autoAddHistory = True
}

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runInputT settings $ case opts of
    Options { errorOpt = True } -> printError "Unrecognized option"
    Options { helpOpt = True } -> helpAction
    _ -> loop $ modeOpt opts
