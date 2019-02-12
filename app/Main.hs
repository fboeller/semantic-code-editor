module Main where

import PromptShow (printPrompt)
import AppState (AppState, initialState, leafFocus, clearOutput, output, running)
import Output (printOutput)
import Evaluation (processCommand, processJavaInput)
import System.IO (hFlush, stdout)
import System.Console.ANSI

import Control.Lens
          
readInput :: IO String
readInput = hFlush stdout >> getLine
     
prompt :: AppState -> IO ()
prompt state = do
  setSGR [SetColor Foreground Vivid Blue]
  putStr $ printPrompt $ state ^.to leafFocus
  putStr " > "
  setSGR [Reset]
  
step :: AppState -> IO (Maybe AppState)
step state = do
  let cleanState = clearOutput state
  prompt cleanState
  input <- readInput 
  newState <- processCommand input cleanState
  printOutput $ newState ^. output
  if newState ^. running then
    step newState
  else
    return Nothing

main :: IO ()
main = do
  loadedState <- processJavaInput "./data" initialState
  printOutput $ loadedState ^. output
  step loadedState
  return ()
