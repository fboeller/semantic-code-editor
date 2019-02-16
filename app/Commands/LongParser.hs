module Commands.LongParser where

import Commands.Types

import Text.ParserCombinators.Parsec (Parser, choice, string, (<?>))
import Control.Lens hiding (List)

keywordP :: Keyword a -> Parser a
keywordP keyword = string (keyword ^. word) *> pure (keyword ^. model) <?> (keyword ^. description)

keywordFromTuple :: (String, a, String) -> Keyword a
keywordFromTuple (word, model, description) =
  Keyword { _word = word
          , _model = model
          , _description = description
          }

firstCommands :: [Keyword FirstCommand]
firstCommands = keywordFromTuple <$>
  [ ("read", Read, "read (Print the content of an element)")
  , ("focus", Focus, "focus (Focus a given element)")
  , ("list", List, "list (List a tree of elements matching the given selector)")
  ]

firstCommand :: Parser FirstCommand
firstCommand = choice (keywordP <$> firstCommands)
  <?> "a first command 'read', 'focus' or 'list'"

elementTypes :: [Keyword ElementType]
elementTypes = keywordFromTuple <$>
  [ ("class", Class, "")
  , ("interface", Interface, "")
  , ("enum", Enum, "")
  , ("method", Method, "")
  , ("function", Function, "")
  , ("variable", Variable, "")
  , ("parameter", Parameter, "")
  , ("extension", Extension, "")
  , ("name", Name, "")
  , ("type", Type, "")
  , ("definition", Definition, "")
  ]

elementType :: Parser ElementType
elementType = choice (keywordP <$> elementTypes)
  <?> "an element type 'class', 'interface', 'enum', 'method', 'focus', 'variable', 'parameter', 'extension', 'name', 'type' or 'definition'"
