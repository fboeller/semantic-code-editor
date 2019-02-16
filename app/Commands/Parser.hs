module Commands.Parser where

import Commands.Types
import Commands.Lexer
import qualified Commands.ShortParser as SP
import qualified Commands.LongParser as LP

import Text.ParserCombinators.Parsec (Parser, choice, between, char, string, parse, try, (<|>), (<?>), newline, sepBy)
import Control.Applicative hiding ((<|>))

data ParserType = Long | Short

firstCommand :: ParserType -> Parser FirstCommand
firstCommand Long = LP.firstCommand
firstCommand Short = SP.firstCommand

elementType :: ParserType -> Parser ElementType
elementType Long = LP.elementType
elementType Short = SP.elementType

runParser :: ParserType -> String -> Either String Command
runParser parserType str =
  let commandParser = command (firstCommand parserType) (elementType parserType) in
    case parse commandParser "Parser for commands" (str ++ ";") of
      Left err -> Left $ show err
      Right val -> Right val

-- Parser for a sequence of filters by element type or/and a search term
-- Each of this filters is meant to filter on a subsequent element in the element tree
selections :: Parser ElementType -> Parser [(Maybe ElementType, Maybe String)]
selections elementType = many $ lexeme (selection elementType)

selection :: Parser ElementType -> Parser (Maybe ElementType, Maybe String)
selection elementType = (between (char '(') (char ')') (selection elementType))
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

metaCommand :: Parser MetaCommand
metaCommand = (LoadFile <$> dataDirPath <$> (metaChar *> char 'l' *> space *> identifier) <* closer)
  <||> (LoadFile <$> (metaChar *> char 'l' *> space *> stringLiteral) <* closer)

command :: Parser FirstCommand -> Parser ElementType -> Parser Command
command firstCommand elementType = quit
  <||> emptyCommand
  <||> (IndexSingle Focus <$> indexPath <* closer <?> "a number to focus a result")
  <||> (Meta <$> metaCommand)
  <||> (Double <$> lexeme firstCommand <*> (selections elementType) <* closer)
  <||> (PathSingle <$> firstCommand <* space <*> path <* closer)
  <||> (IndexSingle <$> firstCommand <* space <*> (integer `sepBy` char '.') <* closer)
