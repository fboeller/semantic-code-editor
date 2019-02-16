module Commands.ShortParser where

import Commands.Types
import Commands.Lexer

import Text.ParserCombinators.Parsec (Parser, choice, between, char, string, parse, try, (<|>), (<?>), newline, sepBy)
import Control.Applicative hiding ((<|>))

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

-- Parser for a sequence of filters by element type or/and a search term
-- Each of this filters is meant to filter on a subsequent element in the element tree
selections :: Parser [(Maybe ElementType, Maybe String)]
selections = many $ lexeme selection

selection :: Parser (Maybe ElementType, Maybe String)
selection = (between (char '(') (char ')') selection)
  <||> ((,) <$> (pure <$> lexeme elementType <* lexeme (char '|')) <*> (pure <$> lexeme stringLiteral))
  <||> ((,) <$> (pure <$> lexeme elementType) <*> pure Nothing)
  <||> ((,) <$> pure Nothing <*> (pure <$> lexeme stringLiteral))
  <||> (char '*' *> pure (Nothing, Nothing))

path :: Parser Path
path = (string ".." *> pure Upper)
  <||> (char '/' *> pure Root)
  <?> "path"

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

quit :: Parser Command
quit = char 'q' *> pure Exit <* closer
  <?> "a 'q' to quit the program"

emptyCommand :: Parser Command
emptyCommand = (mempty :: Parser String) *> pure Empty <* closer

indexPath :: Parser [Integer]
indexPath = integer `sepBy` char '.'

dataDirPath :: String -> FilePath
dataDirPath className = "data/" ++ className ++ ".java"

command :: Parser Command
command = quit
  <||> emptyCommand
  <||> (IndexSingle Focus <$> indexPath <* closer <?> "a number to focus a result")
  <||> (Meta <$> LoadFile <$> dataDirPath <$> (metaChar *> char 'l' *> space *> identifier) <* closer)
  <||> (Meta <$> LoadFile <$> (metaChar *> char 'l' *> space *> stringLiteral) <* closer)
  <||> (Double <$> lexeme firstCommand <*> selections <* closer)
  <||> (PathSingle <$> firstCommand <* space <*> path <* closer)
  <||> (IndexSingle <$> firstCommand <* space <*> (integer `sepBy` char '.') <* closer)
