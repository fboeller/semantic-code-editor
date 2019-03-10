{-# LANGUAGE TemplateHaskell #-}

module Java.Types where

import Prelude hiding (Enum)
import Language.Java.Syntax (BlockStmt)
import Control.Lens

newtype Identifier =
  Identifier { _idName :: String
             }
  deriving (Show, Eq)
makeLenses ''Identifier

newtype Datatype =
  Datatype { _datatypeName :: String
           }
  deriving (Show, Eq)
makeLenses ''Datatype

data Visibility = Private | Protected | Public
  deriving (Show, Eq)

data Field =
  Field { _fieldName :: Identifier
        , _fieldType :: Datatype
        , _fieldVisibility :: Visibility
        , _fieldStatic :: Bool
        , _fieldFinal :: Bool
        }
  deriving (Show, Eq)
makeLenses ''Field

data Parameter =
  Parameter { _parameterName :: Identifier
            , _parameterType :: Datatype
            }
  deriving (Show, Eq)
makeLenses ''Parameter

data Method =
  Method { _methodName :: Identifier
         , _methodParameters :: [Parameter]
         , _methodReturnType :: Datatype
         , _methodVisibility :: Visibility
         , _methodBody :: Maybe [BlockStmt] -- TODO Include syntactic information
         , _methodStatic :: Bool
         }
  deriving (Show, Eq)
makeLenses ''Method

data Constructor =
  Constructor { _constructorName :: Identifier
              , _constructorParameters :: [Parameter]
              , _constructorVisibility :: Visibility
              , _constructorBody :: [BlockStmt] -- TODO Include syntactic information
              }
  deriving (Show, Eq)
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
  deriving (Show, Eq)
makeLenses ''Class

data Interface =
  Interface { _interfaceName :: Identifier
            , _interfaceMethods :: [Method]
            , _interfaceVisibility :: Visibility
            , _interfaceExtends :: [Identifier]
        }
  deriving (Show, Eq)
makeLenses ''Interface

data Enum =
  Enum { _enumName :: Identifier
       , _enumConstants :: [Identifier]
       , _enumFields :: [Field]
       , _enumMethods :: [Method]
       , _enumVisibility :: Visibility
       }
  deriving (Show, Eq)
makeLenses ''Enum

data JavaFile =
  JavaFile { _fileName :: FilePath
           , _packageName :: Identifier
           , _classes :: [Class]
           , _interfaces :: [Interface]
           , _enums :: [Enum]
           }
  deriving (Show, Eq)
makeLenses ''JavaFile

data Project =
  Project { _srcDir :: FilePath
          , _javaFiles :: [JavaFile]
          }
  deriving (Show, Eq)
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
  deriving (Show, Eq)
