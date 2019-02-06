{-# LANGUAGE TemplateHaskell #-}

module AppState where

import qualified Java as J
import Focus
import PromptShow
import Control.Lens
import System.Console.ANSI

data Output =
  ResultList [J.Element] |
  Other (IO ()) |
  Error (IO ())
  
data AppState =
  AppState { _program :: J.Project
           , _focus :: Focus
           , _lastOutput :: Output
           , _output :: Output
           }

makeLenses ''AppState

-- Yields the element of the focus which is most down in the hierarchy
leafFocus :: AppState -> J.Element
leafFocus state =
  case state ^. focus of
    [] -> J.EProject $ state ^. program
    leaf:_ -> leaf

-- Sets the last output to the current output if it is not an error and clears the current output
clearOutput :: AppState -> AppState
clearOutput state = state
  & lastOutput .~ newLastOutput (state ^. output)
  & output .~ Other mempty
  where
    newLastOutput (Error io) = state ^. lastOutput
    newLastOutput newOutput = newOutput

printOutput :: AppState -> IO ()
printOutput state =
  case state ^. output of
    ResultList elements -> putStr $ unlines $ withIndex $ printSignature <$> elements
    Other io -> io
    Error io -> do
      setSGR [SetColor Foreground Vivid Red]
      io
      setSGR [Reset]
  where
    withIndex :: [String] -> [String]
    withIndex = zipWith (\i e -> show i ++ ": " ++ e) [1..]
              
initialState :: AppState
initialState =
  AppState { _program = J.Project { J._javaFiles =
                                      [ J.JavaFile { J._fileName = ""
                                                   , J._packageName = J.Identifier { J._idName = "java.abc" }
                                                   , J._classes = [ car, bus ]
                                                   }
                                      ]
                                  }
           , _focus = []
           , _lastOutput = Other mempty
           , _output = Other mempty
           }
  where
    car = J.Class { J._className = J.Identifier { J._idName = "Car" }
                  , J._classFields = [ J.Field { J._fieldName = J.Identifier { J._idName = "speed" }
                                               , J._fieldType = J.Datatype { J._datatypeName = "int" }
                                               , J._fieldVisibility = J.Private
                                               }
                                     , J.Field { J._fieldName = J.Identifier { J._idName = "altitude" }
                                               , J._fieldType = J.Datatype { J._datatypeName = "int" }
                                               , J._fieldVisibility = J.Protected
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
