{-# LANGUAGE TemplateHaskell #-}

module Commands.Types where

import Control.Lens

data Keyword a =
  Keyword { _word :: String
          , _model :: a
          , _description :: String
          }
makeLenses ''Keyword

data ParserType = Long
                | Short
  deriving Show

data FirstCommand = Read
                  | Focus
                  | List
  deriving Show

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
  deriving Show

data Path = Upper
          | Root
  deriving Show

data MetaCommand = LoadFile String
                 | SwitchCommandParser ParserType
  deriving Show

data Command = Empty
             | Exit
             | Meta MetaCommand
             | Double FirstCommand [(Maybe ElementType, Maybe String)]
             | IndexSingle FirstCommand [Integer]
             | PathSingle FirstCommand Path
  deriving Show
