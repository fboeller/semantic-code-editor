module Commands.ShortParser where

import Commands.Types

import Text.ParserCombinators.Parsec (Parser, choice, char, (<?>))

firstCommand :: Parser FirstCommand
firstCommand = choice
  [ Read <$ char 'r'
  , Focus <$ char 'f'
  , List <$ char 'l'
  ] <?> "a first command symbol 'r', 'f' or 'l'"

elementType :: Parser ElementType
elementType = choice
  [ Class <$ char 'c'
  , Interface <$ char 'i'
  , Enum <$ char 'e'
  , Method <$ char 'm'
  , Function <$ char 'f'
  , Variable <$ char 'v'
  , Parameter <$ char 'p'
  , Extension <$ char 'x'
  , Name <$ char 'n'
  , Type <$ char 't'
  , Definition <$ char 'd'
  ] <?> "an element type 'c', 'i', 'e', 'm', 'f', 'v', 'p', 'x', 'n', 't' or 'd'"
