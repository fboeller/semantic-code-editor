{-# LANGUAGE TemplateHaskell #-}

module AppState where

import qualified Java as J
import Focus
import Control.Lens

data AppState =
  AppState { _program :: J.Package
           , _focus :: Focus
           , _output :: String
           }

makeLenses ''AppState

leafFocus :: AppState -> J.Element
leafFocus state =
  case state ^. focus of
    [] -> J.EPackage $ state ^. program
    leaf:_ -> leaf
                    
initialState :: AppState
initialState =
  AppState { _program =
             J.Package { J._packageName = J.Identifier { J._idName = "java.abc" }
                       , J._classes = [ car, bus ]
                       }
           , _focus = [J.EClass car]
           , _output = ""
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
