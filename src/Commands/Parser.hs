module Commands.Parser where

import Commands.Types
import Commands.Lexer
import Commands.Util
import qualified Commands.ShortParser as SP
import qualified Commands.LongParser as LP

import Text.ParserCombinators.Parsec (Parser, choice, between, char, string, parse, try, (<|>), (<?>), newline, sepBy, many1)
import Control.Applicative hiding ((<|>))
import Data.Bifunctor

firstCommand :: ParserType -> Parser FirstCommand
firstCommand Long = LP.firstCommand
firstCommand Short = SP.firstCommand

elementType :: ParserType -> Parser ElementType
elementType Long = LP.elementType
elementType Short = SP.elementType

runParser :: ParserType -> String -> Either String Command
runParser parserType =
  first show . parse commandParser "Parser for commands" . (++";")
  where commandParser = command (firstCommand parserType) (elementType parserType)

selections elementType =
  many1 (selection elementType <* many space)
  <?> "selections"

selection :: Parser ElementType -> Parser (Maybe ElementType, Maybe String)
selection elementType = trychoice
  [ between (char '(' <* spaces) (spaces *> char ')') (selection elementType)
  , (,) <$> (Just <$> elementType) <* (spaces *> char '|' <* spaces) <*> (Just <$> searchTerm)
  , (,) <$> (Just <$> elementType) <*> pure Nothing
  , (,) <$> pure Nothing <*> (Just <$> searchTerm)
  , (Nothing, Nothing) <$ char '*'
  ] <?> "selection"

searchTerm :: Parser String
searchTerm = char '\"' *> identifier <* char '\"'

path :: Parser Path
path = (Upper <$ string "..")
  <||> (Root <$ char '/')
  <?> "path"

quit :: Parser Command
quit = Exit <$ char 'q'
  <?> "a 'q' to quit the program"

emptyCommand :: Parser Command
emptyCommand = Empty <$ (mempty :: Parser String)

indexPath :: Parser [Integer]
indexPath = integer `sepBy` char '.'
  <?> "index path"

dataDirPath :: String -> FilePath
dataDirPath className = "data/" ++ className ++ ".java"

parserType :: Parser ParserType
parserType = (Long <$ string "long")
  <||> (Short <$ string "short")

metaCommand :: Parser MetaCommand
metaCommand = trychoice
  [ closed $ LoadFile . dataDirPath <$> (metaChar *> lexeme (string "load") *> identifier)
  , closed $ LoadFile <$> (metaChar *> lexeme (string "load") *> stringLiteral)
  , closed $ SwitchCommandParser <$> (metaChar *> lexeme (string "switch") *> parserType)
  ]

closed p = p <* closer

command :: Parser FirstCommand -> Parser ElementType -> Parser Command
command firstCommand elementType = many space *> trychoice
  [ closed quit
  , closed emptyCommand
  , closed $ Double <$> firstCommand <*> pure []
  , closed $ IndexSingle <$> pure Focus <*> indexPath
  , closed $ Double <$> firstCommand <* many1 space <*> selections elementType
  , closed $ IndexSingle <$> firstCommand <* many1 space <*> indexPath
  , closed $ PathSingle <$> firstCommand <* many1 space <*> path
  , Meta <$> metaCommand
  ]
