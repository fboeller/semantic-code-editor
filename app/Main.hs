module Main where

import Java.Printer (printPrompt)
import AppState (AppState, initialState, leafFocus, clearOutput, output, running)
import Output (printOutput)
import Evaluation (processCommand, processJavaInput)

import Control.Lens
import System.Console.Haskeline
import Control.Monad.Trans.Class (lift)
import Data.List (isPrefixOf)
       
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

commandCompletion :: CompletionFunc IO
commandCompletion = completeWordWithPrev Nothing [' '] possibilities

firstCommands :: [String]
firstCommands = ["read", "focus", "list"]

possibilities :: String -> String -> IO [Completion]
possibilities "" "" = return $ simpleCompletion <$> firstCommands
possibilities "" enteredText =
  return $ simpleCompletion <$> filter (enteredText `isPrefixOf`) firstCommands
possibilities _ _ = return []

main :: IO ()
main = do
  loadedState <- processJavaInput "./data" initialState
  printOutput $ loadedState ^. output
  runInputT settings $ loop loadedState
