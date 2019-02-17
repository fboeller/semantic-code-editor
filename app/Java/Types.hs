{-# LANGUAGE TemplateHaskell #-}

module Java.Types where

import Prelude hiding (Enum)
import Control.Lens

data Identifier =
  Identifier { _idName :: String
             }
  deriving (Show)
makeLenses ''Identifier

data Datatype =
  Datatype { _datatypeName :: String
           }
  deriving (Show)
makeLenses ''Datatype

data Visibility = Private | Protected | Public
  deriving (Show)

data Field =
  Field { _fieldName :: Identifier
        , _fieldType :: Datatype
        , _fieldVisibility :: Visibility
        , _fieldStatic :: Bool
        , _fieldFinal :: Bool
        }
  deriving (Show)
makeLenses ''Field

data Parameter =
  Parameter { _parameterName :: Identifier
            , _parameterType :: Datatype
            }
  deriving (Show)
makeLenses ''Parameter

data Method =
  Method { _methodName :: Identifier
         , _methodParameters :: [Parameter]
         , _methodReturnType :: Datatype
         , _methodVisibility :: Visibility
         , _methodBody :: String -- TODO Include syntactic information
         , _methodStatic :: Bool
         }
  deriving (Show)
makeLenses ''Method

data Constructor =
  Constructor { _constructorName :: Identifier
              , _constructorParameters :: [Parameter]
              , _constructorVisibility :: Visibility
              , _constructorBody :: String -- TODO Include syntactic information
              }
  deriving (Show)
makeLenses ''Constructor

data Class =
  Class { _className :: Identifier
        , _classFields :: [Field]
        , _classMethods :: [Method]
        , _classConstructors :: [Constructor]
        , _classVisibility :: Visibility
        , _classExtends :: Maybe Identifier
        , _classImplements :: [Identifier]
        , _classFinal :: Bool
        }
  deriving (Show)
makeLenses ''Class

data Interface =
  Interface { _interfaceName :: Identifier
            , _interfaceMethods :: [Method]
            , _interfaceVisibility :: Visibility
            , _interfaceExtends :: [Identifier]
        }
  deriving (Show)
makeLenses ''Interface

data Enum =
  Enum { _enumName :: Identifier
       , _enumConstants :: [Identifier]
       , _enumFields :: [Field]
       , _enumMethods :: [Method]
       , _enumVisibility :: Visibility
       }
  deriving (Show)
makeLenses ''Enum

data JavaFile =
  JavaFile { _fileName :: FilePath
           , _packageName :: Identifier
           , _classes :: [Class]
           , _interfaces :: [Interface]
           , _enums :: [Enum]
           }
  deriving (Show)
makeLenses ''JavaFile

data Project =
  Project { _javaFiles :: [JavaFile]
          }
  deriving (Show)
makeLenses ''Project

data Element = EField Field
             | EParameter Parameter
             | EMethod Method
             | EConstructor Constructor
             | EClass Class
             | EInterface Interface
             | EEnum Enum
             | EJavaFile JavaFile
             | EProject Project
             | EName Identifier
             | EType Datatype
