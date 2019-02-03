{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import PromptShow
import qualified Java as J
import Text.Parsec.Error ( ParseError )
import System.IO ( hFlush, stdout )
import Data.Bifunctor ( first )

import Control.Lens
import Control.Monad ( void )
import qualified Graphics.Vty as V

data AppState =
  AppState { _program :: J.Package
           , _focus :: J.Element
           , _output :: String
           }

makeLenses ''AppState

initialState :: AppState
initialState =
  AppState { _program =
             J.Package { J._packageName = J.Identifier { J._idName = "java.abc" }
                       , J._classes = [ car, bus ]
                       }
           , _focus = J.EClass car
           , _output = ""
           }
  where
    car = J.Class { J._className = J.Identifier { J._idName = "Car" }
                  , J._classFields = [ J.Field { J._fieldName = J.Identifier { J._idName = "speed" }
                                               , J._fieldType = J.Datatype { J._datatypeName = "int" }
                                               , J._fieldVisibility = J.Private
                                               }
                                     ]
                  , J._classMethods = [ J.Method { J._methodName = J.Identifier { J._idName = "drive" }
                                                 , J._methodParameters = []
                                                 , J._methodReturnType = J.Datatype { J._datatypeName = "void" }
                                                 , J._methodVisibility = J.Public
                                                 }
                                      ]
                  , J._classVisibility = J.Public
                  }
    bus = J.Class { J._className = J.Identifier { J._idName = "Bus" }
                  , J._classFields = [ J.Field { J._fieldName = J.Identifier { J._idName = "doors" }
                                               , J._fieldType = J.Datatype { J._datatypeName = "int" }
                                               , J._fieldVisibility = J.Private
                                               }
                                     ]
                  , J._classMethods = []
                  , J._classVisibility = J.Public
                  }
          
classes :: J.Element -> [J.Class]
classes (J.EPackage p) = p ^. J.classes
classes _ = []

selectedClasses :: String -> J.Element -> [J.Class]
selectedClasses term package =
  filter (\c -> c ^. J.className ^. J.idName == term) $ classes package

focusClass :: AppState -> AppState
focusClass state = over focus changeFocus state
  where changeFocus oldFocus = J.EClass $ head $ classes oldFocus

eval :: String -> AppState -> Either String AppState
eval "quit" state = Left "Done!"
eval "r" state = Right $ set output (show $ state ^. program) state
eval "lc" state = Right $ set output (unlines $ printClassSignature <$> classes (state ^. focus)) state
eval ('l':'c':' ':searchTerm) state = Right $ set output (unlines $ printClassSignature <$> selectedClasses searchTerm (J.EPackage $ state ^. program)) state
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
  
