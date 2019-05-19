{-# LANGUAGE TupleSections #-}

module Commands.Parser where

import Commands.Types
import Commands.Lexer
import Commands.Util
import qualified Commands.ShortParser as SP
import qualified Commands.LongParser as LP

import Text.ParserCombinators.Parsec hiding (space, spaces)
import Text.Parsec.Error (messageString, showErrorMessages, errorMessages)
import Data.Bifunctor
import Data.Char (isPrint)

helpText = "Usage:\n\
           \  list [<elementType> [&& name \"<string>\"] [&& type \"<string>\"]]...\n\
           \  read [<number> [.<number>]]...\n\
           \  focus [<number> [.<number>]]...\n\
           \  [<number> [.<number>]]...\n\
           \  focus /\n\
           \  focus ..\n\
           \  :switch (long|short)\n\
           \  :load <path>\n\
           \  quit\n\
           \  help\n\
           \\n\
           \where\n\
           \  elementType = (*|class|interface|enum|method|function|variable|parameter|extension|name|type|definition)\n\
           \\n\
           \Examples:\n\
           \  list class method\n\
           \  list (name \"App\")\n\
           \  list class (variable && type \"int\")\n\
           \  list class method (parameter && type \"String\")\n\
           \  list (class && name \"Factory\") method (type \"Builder\") definition\n\
           \  read 1.3\n\
           \  focus 2.4\n\
           \  2.4\n"

firstCommand :: ParserType -> Parser FirstCommand
firstCommand Long = LP.firstCommand
firstCommand Short = SP.firstCommand

elementType :: ParserType -> Parser ElementType
elementType Long = LP.elementType
elementType Short = SP.elementType

runParser :: ParserType -> String -> Either String Command
runParser parserType =
  first showParseError . parse commandParser "Invalid command" . (++";")
  where commandParser = command (firstCommand parserType) (elementType parserType)

showParseError :: ParseError -> String
showParseError err =
  "Unexpected " ++ messageString (head (errorMessages err)) ++ " at position " ++ show (sourceColumn (errorPos err)) ++ ". See 'help'."

type Selection = (Maybe ElementType, Maybe String, Maybe String)

selections :: Parser ElementType -> Parser [Selection]
selections elementType =
  many1 (selection elementType <* many space)
  <?> "a hierarchical selection pattern"

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
  ] <?> "a selection pattern"
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
path = (Upper <$ string ".." <?> "'..' for the parent")
  <||> (Root <$ char '/' <?> "'/' for the root")

quit :: Parser Command
quit = Exit <$ string "quit"
  <?> "'quit' to quit the program"

help :: Parser Command
help = Help <$ string "help"
  <?> "'help' to get more info"

emptyCommand :: Parser Command
emptyCommand = Empty <$ (mempty :: Parser String)

indexPath :: Parser [Integer]
indexPath = integer `sepBy` char '.'
  <?> "an index path like '2.3.5'"

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
command firstCommand elementType = (many space <?> "") *> trychoice
  [ closed quit
  , closed help
  , closed emptyCommand
  , closed $ Double <$> firstCommand <*> pure []
  , closed $ IndexSingle <$> pure Focus <*> indexPath
  , closed $ Double <$> firstCommand <* many1 space <*> selections elementType
  , closed $ IndexSingle <$> firstCommand <* many1 space <*> indexPath
  , closed $ PathSingle <$> firstCommand <* many1 space <*> path
  , Meta <$> metaCommand
  ]
