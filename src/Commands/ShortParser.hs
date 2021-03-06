module Commands.ShortParser where

import Commands.Types

import Text.ParserCombinators.Parsec (Parser, choice, char, (<?>))

firstCommand :: Parser FirstCommand
firstCommand = choice
  [ Read <$ char 'r'
  , Focus <$ char 'f'
  , List <$ char 'l'
  ] <?> "one of the commands 'r' for 'read', 'f' for 'focus' and 'l' for 'list'"

elementType :: Parser ElementType
elementType = choice
  [ Class <$ char 'c'
  , Interface <$ char 'i'
  , Enum <$ char 'e'
  , Method <$ char 'm'
  , Field <$ char 'f'
  , Parameter <$ char 'p'
  , Extension <$ char 'x'
  , Name <$ char 'n'
  , Type <$ char 't'
  , Definition <$ char 'd'
  ] <?> "an element type 'c' for 'class', 'i' for 'interface', 'e' for 'enum', 'm' for 'method', 'f' for 'field', 'p' for 'parameter', 'x' for 'extension', 'n' for 'name', 't' for 'type' or 'd' for 'definition'"
