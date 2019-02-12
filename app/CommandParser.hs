module CommandParser where

import Text.ParserCombinators.Parsec (Parser, choice, between, char, string, parse, try, (<|>), (<?>), newline, sepBy)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Control.Applicative hiding ((<|>))

data FirstCommand = Read | Focus | List
  deriving Show

data ElementType = Class | Method | Function | Variable | Parameter | Extension | Name | Type
  deriving Show

data Path = Upper | Root
  deriving Show

data MetaCommand =
  LoadFile String
  deriving Show

data Command =
  Empty |
  Exit |
  Meta MetaCommand |
  Double FirstCommand [(Maybe ElementType, Maybe String)] |
  IndexSingle FirstCommand [Integer] |
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
  ] <?> "a first command symbol 'r', 'f' or 'l'"

secondCommand :: Parser ElementType
secondCommand = choice
  [ char 'c' *> pure Class
  , char 'm' *> pure Method
  , char 'f' *> pure Function
  , char 'v' *> pure Variable
  , char 'p' *> pure Parameter
  , char 'e' *> pure Extension
  , char 'n' *> pure Name
  , char 't' *> pure Type
  ] <?> "a second command symbol 'c', 'm', 'f', 'v', 'p', 'e' or 'n'"

selectionExpression :: Parser [(Maybe ElementType, Maybe String)]
selectionExpression = many (lexeme single)
  where
    single :: Parser (Maybe ElementType, Maybe String)
    single = (between (char '(') (char ')') single)
      <||> ((,) <$> (pure <$> lexeme secondCommand <* lexeme (char '|')) <*> (pure <$> lexeme stringLiteral))
      <||> ((,) <$> (pure <$> lexeme secondCommand) <*> pure Nothing)
      <||> ((,) <$> pure Nothing <*> (pure <$> lexeme stringLiteral))
      <||> (char '*' *> pure (Nothing, Nothing))

path :: Parser Path
path = (string ".." *> pure Upper)
  <||> (char '/' *> pure Root)
  <?> "path"

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

command :: Parser Command
command = (char 'q' *> pure Exit <* closer <?> "a 'q' to quit the program")
  <||> ((mempty :: Parser String) *> pure Empty <* closer)
  <||> (IndexSingle Focus <$> (integer `sepBy` char '.') <* closer <?> "a number to focus a result")
  <||> (Meta <$> LoadFile <$> (\className -> "data/" ++ className ++ ".java") <$> (metaChar *> string "l" *> space *> identifier) <* closer)
  <||> (Meta <$> LoadFile <$> (metaChar *> string "l" *> space *> stringLiteral) <* closer)
  <||> (Double <$> lexeme firstCommand <*> selectionExpression <* closer)
  <||> (PathSingle <$> firstCommand <* space <*> path <* closer)
  <||> (IndexSingle <$> firstCommand <* space <*> (integer `sepBy` char '.') <* closer)

-- Lexer

lexer = P.makeTokenParser emptyDef

integer = P.integer lexer 
stringLiteral = P.stringLiteral lexer
identifier = P.identifier lexer
lexeme = P.lexeme lexer
semi = P.semi lexer
whiteSpace = P.whiteSpace lexer
space = char ' ' :: Parser Char
metaChar = char ':' <?> "the meta symbol ':' to start a meta command" :: Parser Char
closer = whiteSpace <* semi
