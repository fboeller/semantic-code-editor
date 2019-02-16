module Commands.LongParser where

import Commands.Types

import Text.ParserCombinators.Parsec (Parser, choice, string, (<?>))

firstCommand :: Parser FirstCommand
firstCommand = choice
  [ string "read" *> pure Read
  , string "focus" *> pure Focus
  , string "list" *> pure List
  ] <?> "a first command 'read', 'focus' or 'list'"

elementType :: Parser ElementType
elementType = choice
  [ string "class" *> pure Class
  , string "interface" *> pure Interface
  , string "enum" *> pure Enum
  , string "method" *> pure Method
  , string "function" *> pure Function
  , string "variable" *> pure Variable
  , string "parameter" *> pure Parameter
  , string "extension" *> pure Extension
  , string "name" *> pure Name
  , string "type" *> pure Type
  , string "definition" *> pure Definition
  ] <?> "an element type 'class', 'interface', 'enum', 'method', 'focus', 'variable', 'parameter', 'extension', 'name', 'type' or 'definition'"
