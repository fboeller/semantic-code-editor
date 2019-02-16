module Main where

import Java.Printer (printPrompt)
import AppState (AppState, initialState, leafFocus, clearOutput, output, running)
import Output (printOutput)
import Evaluation (processCommand, processJavaInput)
import Commands.AutoComplete

import Control.Lens
import System.Console.Haskeline
import Control.Monad.Trans.Class (lift)
       
loop :: AppState -> InputT IO ()
loop state = do
  minput <- getInputLine $ (printPrompt $ state ^.to leafFocus) ++ " > "
  case minput of
    Nothing -> return ()
    Just input -> do
      let cleanState = clearOutput state
      newState <- lift $ processCommand input cleanState
      lift $ printOutput $ newState ^. output
      if newState ^. running then
        loop newState
      else
        return ()

settings :: Settings IO
settings = setComplete commandCompletion defaultSettings

main :: IO ()
main = do
  loadedState <- processJavaInput "./data" initialState
  printOutput $ loadedState ^. output
  runInputT settings $ loop loadedState
