module CommandParser where

import Text.ParserCombinators.Parsec (Parser, choice, char, string, parse, try, (<|>), newline)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Control.Applicative hiding ((<|>))

data FirstCommand = Read | Focus | List
  deriving Show

data ElementType = Class | Method | Function | Variable
  deriving Show

data Path = Upper
  deriving Show

data MetaCommand =
  LoadFile String
  deriving Show

data Command =
  Empty |
  Exit |
  Meta MetaCommand |
  Index Integer |
  Single FirstCommand |
  Double FirstCommand ElementType |
  IndexSingle FirstCommand Integer |
  TermSingle FirstCommand String |
  TermDouble FirstCommand ElementType String |
  PathSingle FirstCommand Path
  deriving Show

runParser :: String -> Either String Command
runParser str = case parse command "Parser for commands" (str ++ ";") of
  Left err -> Left $ show err
  Right val -> Right val

firstCommand :: Parser FirstCommand
firstCommand = choice
  [ char 'r' *> pure Read
  , char 'f' *> pure Focus
  , char 'l' *> pure List
  ]

secondCommand :: Parser ElementType
secondCommand = choice
  [ char 'c' *> pure Class
  , char 'm' *> pure Method
  , char 'f' *> pure Function
  , char 'v' *> pure Variable
  ]

path :: Parser Path
path = string ".." *> pure Upper

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

command :: Parser Command
command = (char 'q' *> pure Exit)
  <||> (Index <$> integer)
  <||> (Meta <$> LoadFile <$> (\className -> "data/" ++ className ++ ".java") <$> (char ':' *> string "l" *> space *> identifier))
  <||> (Meta <$> LoadFile <$> (char ':' *> string "l" *> space *> stringLiteral))
  <||> (TermDouble <$> firstCommand <*> secondCommand <* space <*> identifier)
  <||> (Double <$> firstCommand <*> secondCommand)
  <||> (TermSingle <$> firstCommand <* space <*> identifier)
  <||> (PathSingle <$> firstCommand <* space <*> path)
  <||> (IndexSingle <$> firstCommand <* space <*> integer)
  <||> (Single <$> firstCommand)
  <||> ((mempty :: Parser String) *> pure Empty)
  <* whiteSpace
  <* semi

-- Lexer

lexer = P.makeTokenParser emptyDef

integer = P.integer lexer 
stringLiteral = P.stringLiteral lexer
identifier = P.identifier lexer
lexeme = P.lexeme lexer
semi = P.semi lexer
whiteSpace = P.whiteSpace lexer
space = char ' ' :: Parser Char
