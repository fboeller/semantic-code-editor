{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import PromptShow
import AppState (AppState, program, focus, output, initialState, leafFocus)
import qualified Actions as A
import Text.Parsec.Error ( ParseError )
import System.IO ( hFlush, stdout )
import Data.Bifunctor ( first )

import Control.Lens
import Control.Monad ( void )
import qualified Graphics.Vty as V
          
eval :: String -> AppState -> Either String AppState
eval "quit" state = Left "Done!"
eval "r" state = Right $ A.read state
eval "lc" state = Right $ A.listClasses state
eval ('l':'c':' ':searchTerm) state = Right $ A.listSelectedClasses searchTerm state
eval "lv" state = Right $ A.listVariables state
eval "fc" state = Right $ A.focusClass state
eval "f .." state = Right $ A.focusUp state
eval input state = Right $ set output input state

readInput :: String -> IO String
readInput prompt = putStr (prompt ++ " > ")
        >> hFlush stdout
        >> getLine
     
printState :: AppState -> String
printState state =
  case state ^. output of
    "" -> ""
    str -> str ++ "\n"

prompt :: AppState -> String
prompt state = printSignature $ state ^.to leafFocus

step :: AppState -> IO (Maybe AppState)
step state = do
  input <- readInput $ prompt state
  let maybeState = eval input state
  case maybeState of
    Right newState -> do
      putStr $ printState newState
      step newState
    Left _ -> do
      putStrLn "Exit"
      return Nothing

main :: IO ()
main = void $ step initialState
  
