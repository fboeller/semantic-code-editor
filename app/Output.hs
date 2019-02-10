module Output where

import qualified Java as J
import PromptShow

import Control.Lens
import Data.Tree
import System.Console.ANSI

data Output =
  ResultList [J.Element] |
  ResultTree (Tree J.Element) |
  Other (IO ()) |
  Error (IO ())
  
printOutput :: Output -> IO ()
printOutput output =
  case output of
    ResultList elements -> putStr $ unlines $ withIndex $ printSignature <$> elements
    ResultTree elements -> putStr $ drawTree $ printSignature <$> elements
    Other io -> io
    Error io -> do
      setSGR [SetColor Foreground Vivid Red]
      io
      setSGR [Reset]
  where
    withIndex :: [String] -> [String]
    withIndex = zipWith (\i e -> show i ++ ": " ++ e) [1..]
