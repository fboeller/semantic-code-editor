{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import PromptShow
import qualified Java as J
import qualified JavaAccessors as JA
import AppState (AppState, program, focus, output, initialState)
import Actions (focusClass)
import Text.Parsec.Error ( ParseError )
import System.IO ( hFlush, stdout )
import Data.Bifunctor ( first )

import Control.Lens
import Control.Monad ( void )
import qualified Graphics.Vty as V
          
eval :: String -> AppState -> Either String AppState
eval "quit" state = Left "Done!"
eval "r" state = Right $ set output (show $ state ^. program) state
eval "lc" state = Right $ set output (unlines $ printClassSignature <$> JA.classes (state ^. focus)) state
eval ('l':'c':' ':searchTerm) state = Right $ set output (unlines $ printClassSignature <$> JA.selectedClasses searchTerm (J.EPackage $ state ^. program)) state
eval "fc" state = Right $ focusClass state
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
prompt state = printSignature $ state ^. focus

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
  
