module CommandParser where

import Text.ParserCombinators.Parsec (Parser, choice, char, string, parse, try, (<|>), newline)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Control.Applicative hiding ((<|>))

data FirstCommand = Read | Focus | List
  deriving Show

data SecondCommand = Class | Method | Function | Variable
  deriving Show

data Path = Upper
  deriving Show

data Command =
  Empty |
  Exit |
  Index Integer |
  Single FirstCommand |
  Double FirstCommand SecondCommand |
  IndexSingle FirstCommand Integer |
  TermSingle FirstCommand String |
  TermDouble FirstCommand SecondCommand String |
  PathSingle FirstCommand Path
  deriving Show

runParser :: String -> Either String Command
runParser str = case parse command "Parser for commands" str of
  Left err -> Left $ show err
  Right val -> Right val

firstCommand :: Parser FirstCommand
firstCommand = choice
  [ lexeme (char 'r') *> pure Read
  , lexeme (char 'f') *> pure Focus
  , lexeme (char 'l') *> pure List
  ]

secondCommand :: Parser SecondCommand
secondCommand = choice
  [ lexeme (char 'c') *> pure Class
  , lexeme (char 'm') *> pure Method
  , lexeme (char 'f') *> pure Function
  , lexeme (char 'v') *> pure Variable
  ]

path :: Parser Path
path = lexeme (string "..") *> pure Upper

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

command :: Parser Command
command = (lexeme (char 'q') *> pure Exit)
  <||> (Index <$> integer)
  <||> (TermDouble <$> firstCommand <*> secondCommand <*> stringLiteral)
  <||> (Double <$> firstCommand <*> secondCommand)
  <||> (PathSingle <$> firstCommand <*> path)
  <||> (TermSingle <$> firstCommand <*> stringLiteral)
  <||> (IndexSingle <$> firstCommand <*> integer)
  <||> (Single <$> firstCommand)
  <||> ((mempty :: Parser String) *> pure Empty)

-- Lexer

lexer = P.makeTokenParser emptyDef

integer = P.integer lexer 
stringLiteral = P.stringLiteral lexer
lexeme = P.lexeme lexer
