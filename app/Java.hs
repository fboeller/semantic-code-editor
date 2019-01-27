{-# LANGUAGE TemplateHaskell #-}

module Java where

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
         }
  deriving (Show)
makeLenses ''Method

data Class =
  Class { _className :: Identifier
        , _classFields :: [Field]
        , _classMethods :: [Method]
        , _classVisibility :: Visibility
        }
  deriving (Show)
makeLenses ''Class

data Package =
  Package { _packageName :: Identifier
          , _classes :: [Class]
          }
  deriving (Show)
makeLenses ''Package
