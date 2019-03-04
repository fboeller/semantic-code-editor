{-# LANGUAGE TemplateHaskell #-}

module Commands.Types where

import Control.Lens

data Keyword a =
  Keyword { _word :: String
          , _model :: a
          , _description :: String
          }
  deriving (Eq, Show)
makeLenses ''Keyword

data ParserType = Long
                | Short
  deriving (Eq, Show)

data FirstCommand = Read
                  | Focus
                  | List
  deriving (Eq, Show)

data ElementType = Class
                 | Interface
                 | Enum
                 | Method
                 | Function
                 | Variable
                 | Parameter
                 | Extension
                 | Name
                 | Type
                 | Definition
  deriving (Eq, Show, Ord)

data Path = Upper
          | Root
  deriving (Eq, Show)

data MetaCommand = LoadFile String
                 | SwitchCommandParser ParserType
  deriving (Eq, Show)

data Command = Empty
             | Exit
             | Meta MetaCommand
             | Double FirstCommand [(Maybe ElementType, Maybe String, Maybe String)]
             | IndexSingle FirstCommand [Integer]
             | PathSingle FirstCommand Path
  deriving (Eq, Show)
