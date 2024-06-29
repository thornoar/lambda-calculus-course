module Main where

import Lambda
import System.Console.Haskeline
-- import Control.Monad.Catch (handle)

data Mode = SILENT | REPEAT | PRINT | REDUCE | STEPS | EQUIV | SHOW | READ | TOFORMAL | TOINFORMAL | FORMAT
  deriving (Read, Show)

color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m"

printGeneral :: (String -> InputT IO ()) -> Maybe String -> InputT IO ()
printGeneral f Nothing = f $ color "1;31" "error:" ++ " incorrect syntax."
printGeneral f (Just str) = f . ("| " ++) . color "34" $ str

printLn :: Maybe String -> InputT IO ()
printLn = printGeneral outputStrLn

printReturn :: (String -> Maybe String) -> String -> InputT IO (Maybe String)
printReturn f str = do
  let res = f str
  printLn res
  return res

help :: InputT IO ()
help = do
  outputStrLn "usage: [:rp | :rs | :r | :p | :s] [LAMBDA]"

evalOnce :: Mode -> String -> InputT IO (Maybe String)
evalOnce SILENT = return . Just
evalOnce REPEAT = printReturn Just
evalOnce PRINT = printReturn (fmap unparse' . parse')
evalOnce REDUCE = printReturn (fmap (unparse' . reduce) . parse')
evalOnce STEPS = print' . parse'
  where
    print' :: Maybe Lambda -> InputT IO (Maybe String)
    print' Nothing = return Nothing
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
evalOnce EQUIV = showEquiv
  where
    print' :: Maybe Bool -> InputT IO (Maybe String)
    print' mb = do
      let res = show <$> mb
      printLn res
      return res
    showEquiv :: String -> InputT IO (Maybe String)
    showEquiv str1 = do
      let ml1 = parse' str1
      str2 <- getInputLine $ "  (" ++ color "33" "AND" ++ "): "
      case str2 of
        Nothing -> return Nothing
        Just [] -> return Nothing
        _ -> do
          let ml2 = str2 >>= parse'
          print' $ raise equiv' ml1 ml2
evalOnce SHOW = printReturn (fmap show . parse')
evalOnce READ = printReturn (fmap unparse' . read)
evalOnce TOFORMAL = printReturn (fmap unparseFormal . parse')
evalOnce TOINFORMAL = printReturn (fmap unparse' . parse')
evalOnce FORMAT = printReturn (fmap (unparse . adjustBoundVars) . parse')

pipe :: Mode -> Mode -> String -> InputT IO (Maybe String)
pipe basemode curmode str = do
  val <- eval basemode str
  case val of
    Nothing -> return Nothing
    (Just val') -> evalOnce curmode val'


eval :: Mode -> String -> InputT IO (Maybe String)
eval mode str = case str of
  [] -> return Nothing
  ('#':'s':' ':rest) -> pipe SILENT SILENT rest
  ('#':'r':'p':' ':rest) -> pipe REPEAT mode rest
  ('#':'p':' ':rest) -> pipe PRINT mode rest
  ('#':'r':' ':rest) -> pipe REDUCE mode rest
  ('#':'r':'s':' ':rest) -> pipe STEPS mode rest
  ('#':'e':'q':' ':rest) -> pipe EQUIV mode rest
  ('#':'s':'h':' ':rest) -> pipe SHOW SILENT rest
  ('#':'r':'d':' ':rest) -> pipe READ mode rest
  ('#':'t':'f':' ':rest) -> pipe TOFORMAL mode rest
  ('#':'t':'i':' ':rest) -> pipe TOINFORMAL mode rest
  ('#':'f':' ':rest) -> pipe FORMAT mode rest
  -- ('#':rest) -> return Nothing
  input -> evalOnce mode input

loop :: Mode -> InputT IO (Maybe String)
loop mode = do
  minput <- getInputLine $ "(" ++ color "33" (show mode) ++ "): "
  case minput of
    Nothing -> return Nothing
    Just "" -> loop mode
    Just "#quit" -> return Nothing
    Just "#q" -> return Nothing
    Just "#help" -> help >> loop mode
    Just "#h" -> help >> loop mode
    Just "#silent" -> loop SILENT
    Just "#repeat" -> loop REPEAT
    Just "#print" -> loop PRINT
    Just "#reduce" -> loop REDUCE
    Just "#steps" -> loop STEPS
    Just "#equiv" -> loop EQUIV
    Just "#show" -> loop SHOW
    Just "#read" -> loop READ
    Just "#toformal" -> loop TOFORMAL
    Just "#toinformal" -> loop TOINFORMAL
    Just "#format" -> loop FORMAT
    Just "#s" -> loop SILENT
    Just "#rp" -> loop REPEAT
    Just "#p" -> loop PRINT
    Just "#r" -> loop REDUCE
    Just "#rs" -> loop STEPS
    Just "#eq" -> loop EQUIV
    Just "#sh" -> loop SHOW
    Just "#rd" -> loop READ
    Just "#tf" -> loop TOFORMAL
    Just "#ti" -> loop TOINFORMAL
    Just "#f" -> loop FORMAT
    Just input -> do
      _ <- eval mode input
      loop mode

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just ".lambda-interpreter-history",
  autoAddHistory = True
}

-- interrupt :: InputT IO (Maybe String)
-- interrupt = do
--   outputStrLn $ color "31" "Interrupted."
--   handleInterrupt interrupt $ withInterrupt $ loop REDUCE
interrupt :: InputT IO ()
interrupt = outputStrLn $ color "31" "Interrupted."

main :: IO ()
main = runInputT settings
  $ handleInterrupt interrupt $ withInterrupt
  $ do
    _ <- loop REDUCE
    return ()
