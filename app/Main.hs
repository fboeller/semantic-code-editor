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
import qualified JavaParser as JP
import Text.Parsec.Error ( ParseError )
import System.IO ( hFlush, stdout )
import Data.Bifunctor ( first )
import System.Console.ANSI
import Text.Read (readMaybe)

import Control.Lens
import Control.Monad ( void )
import qualified Graphics.Vty as V
          
eval :: P.Command -> AppState -> AppState
eval P.Exit = set output $ Other $ putStr "Done!"
eval P.Empty = id
eval (P.Meta _) = id
eval (P.Single P.Read) = A.read
eval (P.Single P.List) = A.listAll
eval (P.Single P.Focus) = A.focusAny
eval (P.Double P.List elementType) = A.listElementsOfType elementType
eval (P.TermDouble P.List elementType term) = A.listSelectedElementsOfType elementType term
eval (P.TermSingle P.List term) = A.listSelectedElements term
eval (P.TermDouble P.Focus elementType term) = A.focusFirstSelectedElementOfType elementType term
eval (P.TermSingle P.Focus term) = A.focusFirstSelectedElement term
eval (P.Double P.Focus P.Class) = A.focusClass
eval (P.Double P.Focus P.Method) = A.focusMethod
eval (P.Double P.Focus P.Variable) = A.focusVariable
eval (P.PathSingle P.Focus P.Upper) = A.focusUp
eval (P.Index number) = A.focusLastOutputByIndex (fromInteger number)
eval (P.IndexSingle P.Focus number) = A.focusLastOutputByIndex (fromInteger number)
eval input = set output $ Error $ putStrLn $ "The command '" ++ show input ++ "' is not yet implemented"

evalMeta :: P.Command -> AppState -> IO AppState
evalMeta (P.Meta (P.LoadFile path)) state = processJavaInput path state
evalMeta _ state = return state

processCommand :: String -> AppState -> IO AppState
processCommand input state =
  case P.runParser input of
    Left err -> return $ state & output .~ Error (putStrLn err)
    Right command -> eval command <$> evalMeta command state

processJavaInput :: String -> AppState -> IO AppState
processJavaInput path state = do
  parseResult <- JP.runParserOnPath path
  return $ case parseResult of
    Left err -> state & output .~ Error (putStrLn err)
    Right javaProgram -> state & program .~ javaProgram & focus .~ []

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
  newState <- processCommand input cleanState
  printOutput newState
  step newState

main :: IO ()
main = void $ step initialState
