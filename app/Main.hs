{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import qualified Language.Java.Parser as Java
import qualified Language.Java.Pretty as P
import Language.Java.Syntax ( CompilationUnit(..) )
import Text.Parsec.Error ( ParseError )
import System.IO ( hFlush, stdout )
import Data.Bifunctor ( first )

import Lens.Micro ( (.~), (^.), (&), set, Lens' )
import Lens.Micro.TH ( makeLenses )
import Control.Monad ( void )
import qualified Graphics.Vty as V

programStr = "import java.util.*; public class MyClass { private int abc; public Integer doStuff(String p1) { /* Something */ return 4 + 6; }}"

/* Wrapper for a lens to prevent impredicative polymorphism */
data FocusLens a = FocusLens (Lens' CompilationUnit a)

data AppState a =
  AppState { _program :: CompilationUnit
           , _focus :: FocusLens a
           , _output :: String
           }

makeLenses ''AppState

initialState :: AppState CompilationUnit
initialState =
  AppState { _program = CompilationUnit Nothing [] []
           , _focus = FocusLens id
           , _output = ""
           }

compileProgram :: AppState a -> Either ParseError (AppState a)
compileProgram state = (\comp -> set program comp state) <$> Java.parser Java.compilationUnit programStr

eval' :: String -> AppState a -> Either String (AppState a)
eval' "quit" state = Left "Done!"
eval' "c" state = first show $ compileProgram state
eval' input state = Right $ set output input state

read' :: IO String
read' = putStr "REPL> "
        >> hFlush stdout
        >> getLine
     
print' :: AppState a -> String
print' state = unlines [state ^. output, show $ P.pretty $ state ^. program]

step :: AppState a -> IO (Maybe (AppState a))
step state = do
  input <- read'
  let maybeState = eval' input state
  case maybeState of
    Right newState -> do
      putStrLn $ print' newState
      step newState
    Left _ -> do
      putStrLn "Exit"
      return Nothing

main :: IO ()
main = void $ step initialState
  
