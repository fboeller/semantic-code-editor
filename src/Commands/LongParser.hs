module Commands.LongParser where

import Commands.Types
import Commands.Util

import Text.ParserCombinators.Parsec (Parser, string, (<?>))
import Control.Lens hiding (List)

keywordP :: Keyword a -> Parser a
keywordP keyword = keyword ^. model <$ string (keyword ^. word)
  <?> (keyword ^. description)

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
firstCommand = trychoice (keywordP <$> firstCommands)
  <?> "a first command 'read', 'focus' or 'list'"

elementTypes :: [Keyword ElementType]
elementTypes = keywordFromTuple <$>
  [ ("class", Class, "class")
  , ("interface", Interface, "interface")
  , ("enum", Enum, "enum")
  , ("method", Method, "method")
  , ("variable", Variable, "variable")
  , ("parameter", Parameter, "parameter")
  , ("extension", Extension, "extension")
  , ("name", Name, "name")
  , ("type", Type, "type")
  , ("definition", Definition, "definition")
  ]

elementType :: Parser ElementType
elementType = trychoice (keywordP <$> elementTypes)
  <?> "an element type 'class', 'interface', 'enum', 'method', 'focus', 'variable', 'parameter', 'extension', 'name', 'type' or 'definition'"
