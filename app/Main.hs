{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import PromptShow
import Java as J
import Text.Parsec.Error ( ParseError )
import System.IO ( hFlush, stdout )
import Data.Bifunctor ( first )

import Control.Lens
import Control.Monad ( void )
import qualified Graphics.Vty as V

-- Wrapper for a lens to prevent impredicative polymorphism
data FocusLens a = FocusLens (Lens' J.Package a)

getFocusLens :: FocusLens a -> Lens' J.Package a
getFocusLens (FocusLens lens) = lens

data AppState a =
  AppState { _program :: J.Package
           , _focus :: FocusLens a
           , _output :: String
           }

makeLenses ''AppState

initialState :: AppState J.Package
initialState =
  AppState { _program =
             J.Package { _packageName = J.Identifier { _idName = "java.abc" },
                         _classes = [ Class { _className = J.Identifier { _idName = "MyClass" }
                                            , _classFields = []
                                            , _classMethods = []
                                            , _classVisibility = J.Public
                                            }
                                    ]
                       }
           , _focus = FocusLens id
           , _output = ""
           }

class Focusable a where
  listClasses :: a -> [J.Class]

instance Focusable J.Package where
  listClasses package = package ^. J.classes

instance Focusable J.Class where
  listClasses _ = []

instance Focusable J.Method where
  listClasses _ = []

instance Focusable J.Parameter where
  listClasses _ = []

instance Focusable J.Field where
  listClasses _ = []

eval' :: (PromptShow a) => String -> AppState a -> Either String (AppState a)
eval' "quit" state = Left "Done!"
eval' "r" state = Right $ set output (show $ state ^. program) state
eval' "lc" state = Right $ set output (unlines $ printSignature <$> listClasses (state ^. program)) state
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
  
