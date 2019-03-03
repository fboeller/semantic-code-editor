module Commands.Util where

import Text.ParserCombinators.Parsec

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

trychoice :: [Parser a] -> Parser a
trychoice = foldr1 (<||>)
