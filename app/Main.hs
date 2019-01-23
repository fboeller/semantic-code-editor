module Main where

import Lib
import Language.Java.Parser as Java

program = "import java.util.*; public class MyClass { private int abc; public Integer doStuff(String p1) { /* Something */ return 4 + 6; }}"

main :: IO ()
main =
  putStrLn $ show $ Java.parser Java.compilationUnit program
