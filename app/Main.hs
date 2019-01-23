module Main where

import Lib
import Language.Java.Parser as Java
import Language.Java.Pretty as P
import Text.Parsec.Error (ParseError)

programStr = "import java.util.*; public class MyClass { private int abc; public Integer doStuff(String p1) { /* Something */ return 4 + 6; }}"

main :: IO ()
main =
  putStrLn $ handleParseError $ processAST <$> Java.parser Java.compilationUnit programStr

handleParseError :: Either ParseError String -> String
handleParseError (Right str) = str
handleParseError (Left error) = "ERR: Could not parse program!"

processAST ast =
  show $ P.pretty ast
