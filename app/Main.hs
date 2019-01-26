{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-} 

module Main where

import Lib
import PromptShow
import qualified Language.Java.Parser as Java
import qualified Language.Java.Pretty as P
import qualified Language.Java.Syntax as S
import Text.Parsec.Error ( ParseError )
import System.IO ( hFlush, stdout )
import Data.Bifunctor ( first )

import Lens.Micro ( (.~), (^.), (&), set, Lens' )
import Lens.Micro.TH ( makeLenses )
import Control.Monad ( void )
import qualified Graphics.Vty as V

programStr = "package abc; import java.util.*; public class MyClass { private int abc; public Integer doStuff(String p1) { /* Something */ return 4 + 6; }}"

/* Wrapper for a lens to prevent impredicative polymorphism */
data FocusLens a = FocusLens (Lens' S.CompilationUnit a)

getFocusLens :: FocusLens a -> Lens' S.CompilationUnit a
getFocusLens (FocusLens lens) = lens

data AppState a =
  AppState { _program :: S.CompilationUnit
           , _focus :: FocusLens a
           , _output :: String
           }

makeLenses ''AppState

initialState :: AppState S.CompilationUnit
initialState =
  AppState { _program = S.CompilationUnit Nothing [] []
           , _focus = FocusLens id
           , _output = ""
           }

data Element = Class | Method | Function | Variable | Interface | Parameter | Body | Statement | Annotation | Extension

class Focusable a where
  list_classes :: a -> [S.ClassDecl]

instance Focusable S.CompilationUnit where
  list_classes (S.CompilationUnit _ _ typeDecls) = typeDecls >>= list_classes

instance Focusable S.TypeDecl where
  list_classes (S.ClassTypeDecl classDecl) = [classDecl]
  list_classes _ = []

instance Focusable S.ClassDecl where
  list_classes _ = []

compileProgram :: AppState a -> Either ParseError (AppState a)
compileProgram state = (\comp -> set program comp state) <$> Java.parser Java.compilationUnit programStr

eval' :: (PromptShow a) => String -> AppState a -> Either String (AppState a)
eval' "quit" state = Left "Done!"
eval' "c" state = first show $ compileProgram state
eval' "r" state = Right $ set output (show $ P.pretty $ state ^. program) state
eval' "lc" state = Right $ set output (unlines $ printSignature <$> list_classes (state ^. program)) state
eval' input state = Right $ set output input state

read' :: String -> IO String
read' prompt = putStr (prompt ++ " > ")
        >> hFlush stdout
        >> getLine
     
print' :: AppState a -> String
print' state =
  case state ^. output of
    "" -> ""
    str -> str ++ "\n"

getFocus state = state ^. program ^. (getFocusLens (state ^. focus))

prompt :: (PromptShow a) => AppState a -> String
prompt state = printSignature $ getFocus state

step :: (PromptShow a) => AppState a -> IO (Maybe (AppState a))
step state = do
  input <- read' $ prompt state
  let maybeState = eval' input state
  case maybeState of
    Right newState -> do
      putStr $ print' newState
      step newState
    Left _ -> do
      putStrLn "Exit"
      return Nothing

main :: IO ()
main = void $ step initialState
  
