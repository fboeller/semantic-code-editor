{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import PromptShow
import AppState (AppState, program, focus, output, initialState, leafFocus, clearOutput, Output(..), printOutput)
import qualified Actions as A
import qualified CommandParser as P
import Text.Parsec.Error ( ParseError )
import System.IO ( hFlush, stdout )
import Data.Bifunctor ( first )
import System.Console.ANSI
import Text.Read (readMaybe)

import Control.Lens
import Control.Monad ( void )
import qualified Graphics.Vty as V
          
eval :: P.Command -> AppState -> AppState
eval P.Exit state = state & output .~ (Other $ putStr "Done!")
eval P.Empty state = state
eval (P.Single P.Read) state = A.read state
eval (P.Single P.List) state = A.listAll state
eval (P.Single P.Focus) state = A.focusAny state
eval (P.Double P.List elementType) state = A.listElementsOfType elementType state
eval (P.TermDouble P.List elementType term) state = A.listSelectedElementsOfType elementType term state
eval (P.TermSingle P.List term) state = A.listSelectedElements term state
eval (P.Double P.Focus P.Class) state = A.focusClass state
eval (P.Double P.Focus P.Method) state = A.focusMethod state
eval (P.Double P.Focus P.Variable) state = A.focusVariable state
eval (P.PathSingle P.Focus P.Upper) state = A.focusUp state
eval (P.IndexSingle P.Focus number) state = A.focusLastOutputByIndex (fromInteger number) state
eval input state = state & output .~ (Error $ putStrLn $ "The command '" ++ show input ++ "' is not yet implemented")

process :: String -> AppState -> AppState
process input state =
  case P.runParser input of
    Left err -> state & output .~ Error (putStrLn err)
    Right command -> eval command state

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
  let newState = process input cleanState
  printOutput newState
  step newState

main :: IO ()
main = void $ step initialState
  
