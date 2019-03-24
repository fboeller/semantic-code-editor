{-# LANGUAGE TupleSections #-}

module Commands.Parser where

import Commands.Types
import Commands.Lexer
import Commands.Util
import qualified Commands.ShortParser as SP
import qualified Commands.LongParser as LP

import Text.ParserCombinators.Parsec hiding (space, spaces)
import Data.Bifunctor
import Data.Char (isPrint)

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

type Selection = (Maybe ElementType, Maybe String, Maybe String)

selections :: Parser ElementType -> Parser [Selection]
selections elementType =
  many1 (selection elementType <* many space)
  <?> "selections"

selection :: Parser ElementType -> Parser Selection
selection elementType = trychoice
  [ inBrackets $ selection elementType
  , (Nothing, Nothing, Nothing) <$ char '*'
  , (Nothing, Nothing, ) <$> typeCond
  , (Nothing, , ) <$> nameCond <* and <*> typeCond
  , (Nothing, , Nothing) <$> nameCond
  , ( , , ) <$> elementTypeCond <* and <*> nameCond <* and <*> typeCond
  , ( , Nothing, ) <$> elementTypeCond <* and <*> typeCond
  , ( , , Nothing) <$> elementTypeCond <* and <*> nameCond
  , ( , Nothing, Nothing) <$> elementTypeCond -- Must be last to not match early
  ] <?> "selection"
  where
    elementTypeCond = Just <$> elementType
    typeCond = string "type" *> many1 space *> (Just <$> searchTerm)
    nameCond = (Just <$> searchTerm) <||> (string "name" *> many1 space *> (Just <$> searchTerm))
    and = spaces *> string "&&" <* spaces
    inBrackets = between (char '(' <* spaces) (spaces *> char ')')

quoted :: Parser String -> Parser String
quoted content = quote *> content <* quote
  where quote = char '\"'

searchTerm :: Parser String
searchTerm = quoted identifier <||> quoted mempty

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

parserType :: Parser ParserType
parserType = (Long <$ string "long")
  <|> (Short <$ string "short")

javaFilePath :: Parser FilePath
javaFilePath = quoted fileChars <||> fileChars
  where fileChars = many1 $ satisfy (\c -> isPrint c && c /= ';' && c /= '\"')
        githubPath = string "github:" *> fileChars

closed p = p <* closer

metaCommand :: Parser MetaCommand
metaCommand = trychoice
  [ closed $ metaChar *> (LoadFile <$> (lexeme (string "load") *> javaFilePath))
  , closed $ metaChar *> (SwitchCommandParser <$> (lexeme (string "switch") *> parserType))
  ]

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
