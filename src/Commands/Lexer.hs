module Commands.Lexer where

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, char, (<?>), many, many1, letter, oneOf, alphaNum, (<|>))

languageDef :: P.LanguageDef st
languageDef = P.LanguageDef
  { P.commentStart   = ""
  , P.commentEnd     = ""
  , P.commentLine    = ""
  , P.nestedComments = True
  , P.identStart     = letter <|> char '_'
  , P.identLetter    = alphaNum <|> oneOf "_'"
  , P.opStart        = opLetter emptyDef
  , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , P.reservedOpNames= []
  , P.reservedNames  = []
  , P.caseSensitive  = False
  }
                                     
lexer = P.makeTokenParser languageDef

integer = P.integer lexer 
identifier = P.identifier lexer
stringLiteral = P.stringLiteral lexer
lexeme = P.lexeme lexer
semi = P.semi lexer
whiteSpace = P.whiteSpace lexer
space = char ' ' :: Parser Char
spaces = many space <?> "spaces"
metaChar = char ':' <?> "the meta symbol ':' to start a meta command" :: Parser Char
closer = spaces <* semi <?> "closer"
