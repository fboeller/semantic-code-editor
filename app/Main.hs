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

documentedCompletion :: String -> String -> Completion
documentedCompletion replacement display =
  Completion { replacement = replacement
             , display = display
             , isFinished = True
             }

firstCommands :: [Completion]
firstCommands = uncurry documentedCompletion <$>
  [ ("read", "read (Print the content of an element)")
  , ("focus", "focus (Focus a given element)")
  , ("list", "list (List a tree of elements matching the given selector)")
  ]

metaCommands :: [Completion]
metaCommands = uncurry documentedCompletion <$>
  [ (":load", ":load (Load a Java file or directory containing Java files)")
  , (":switch", ":switch (Switch the mode of the command parser to accept either long or short commands)")
  ]

completesTo :: String -> Completion -> Bool
completesTo text completion = text `isPrefixOf` (replacement completion)

possibilities :: String -> String -> IO [Completion]
possibilities "" "" = return $ firstCommands ++ metaCommands
possibilities "" enteredText =
  return $ filter (enteredText `completesTo`) (firstCommands ++ metaCommands)
possibilities _ _ = return []

main :: IO ()
main = do
  loadedState <- processJavaInput "./data" initialState
  printOutput $ loadedState ^. output
  runInputT settings $ loop loadedState
