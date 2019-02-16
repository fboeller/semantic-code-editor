module Commands.ShortParser where

import Commands.Types

import Text.ParserCombinators.Parsec (Parser, choice, char, (<?>))

firstCommand :: Parser FirstCommand
firstCommand = choice
  [ char 'r' *> pure Read
  , char 'f' *> pure Focus
  , char 'l' *> pure List
  ] <?> "a first command symbol 'r', 'f' or 'l'"

elementType :: Parser ElementType
elementType = choice
  [ char 'c' *> pure Class
  , char 'i' *> pure Interface
  , char 'e' *> pure Enum
  , char 'm' *> pure Method
  , char 'f' *> pure Function
  , char 'v' *> pure Variable
  , char 'p' *> pure Parameter
  , char 'x' *> pure Extension
  , char 'n' *> pure Name
  , char 't' *> pure Type
  , char 'd' *> pure Definition
  ] <?> "an element type 'c', 'i', 'e', 'm', 'f', 'v', 'p', 'x', 'n', 't' or 'd'"
