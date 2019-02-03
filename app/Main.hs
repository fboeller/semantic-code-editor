{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import PromptShow
import AppState (AppState, program, focus, output, initialState, leafFocus, clearOutput, Output(..), printOutput)
import qualified Actions as A
import Text.Parsec.Error ( ParseError )
import System.IO ( hFlush, stdout )
import Data.Bifunctor ( first )
import System.Console.ANSI
import Text.Read (readMaybe)

import Control.Lens
import Control.Monad ( void )
import qualified Graphics.Vty as V
          
eval :: String -> AppState -> AppState
eval "quit" state = state & output .~ (Other $ putStr "Done!")
eval "r" state = A.read state
eval "l" state = A.listAll state
eval "lc" state = A.listClasses state
eval "lm" state = A.listMethods state
eval "lv" state = A.listVariables state
eval ('l':'c':' ':searchTerm) state = A.listSelectedClasses searchTerm state
eval "fc" state = A.focusClass state
eval "fm" state = A.focusMethod state
eval "fv" state = A.focusVariable state
eval "f .." state = A.focusUp state
eval input@('f':' ':term) state =
  case readMaybe term :: Maybe Int of
    Nothing -> failParsing input state 
    Just number -> A.focusLastOutputByIndex number state
eval "" state = state
eval input state = failParsing input state

failParsing :: String -> AppState -> AppState
failParsing input state = set output (Error $ putStrLn $ "Command '" ++ input ++ "' is unknown") state

readInput :: IO String
readInput = hFlush stdout >> getLine
     
prompt :: AppState -> IO ()
prompt state = do
  setSGR [SetColor Foreground Vivid Blue]
  putStr $ printSignature $ state ^.to leafFocus
  putStr " > "
  setSGR [Reset]
  

step :: AppState -> IO (Maybe AppState)
step state = do
  let cleanState = clearOutput state
  prompt cleanState
  input <- readInput 
  let newState = eval input cleanState
  printOutput newState
  step newState

main :: IO ()
main = void $ step initialState
  
