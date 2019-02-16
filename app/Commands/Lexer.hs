module Commands.Lexer where

import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, char, (<?>))
                                     
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
