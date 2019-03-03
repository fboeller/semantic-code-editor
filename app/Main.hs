module Main where

import Java.Printer (printPrompt)
import AppState (AppState, initialState, leafFocus, clearOutput, output, running)
import Output (printOutput)
import Evaluation (processCommand, processJavaInput)
import Commands.AutoComplete

import System.Console.Haskeline
import System.Environment (getArgs)
import Control.Lens
import Control.Monad.Trans.Class (lift)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
       
loop :: AppState -> InputT IO ()
loop state = do
  minput <- getInputLine $ printPrompt (state ^.to leafFocus) ++ " > "
  case minput of
    Nothing -> return ()
    Just input -> do
      let cleanState = clearOutput state
      newState <- lift $ processCommand input cleanState
      lift $ printOutput $ newState ^. output
      when (newState ^. running) $ loop newState

settings :: Settings IO
settings = setComplete commandCompletion defaultSettings

run :: FilePath -> IO ()
run srcPath = do
  loadedState <- processJavaInput srcPath initialState
  printOutput $ loadedState ^. output
  runInputT settings $ loop loadedState

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> run "./data"
    [srcPath] -> run srcPath
    _ -> putStrLn "At most one single source directory or source file is supported."
