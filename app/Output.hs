module Output where

import qualified Java as J
import PromptShow

import Control.Lens ((^.))
import Data.Tree
import System.Console.ANSI

data Output =
  ResultList [J.Element] |
  ResultTree (Tree J.Element) |
  Other (IO ()) |
  Error (IO ())
  
printOutput :: Output -> IO ()
printOutput (ResultList elements) = putStr $ unlines $ withIndex $ printSignature <$> elements
printOutput (ResultTree elements) = putStr $ drawTree $ printSignature <$> elements
printOutput (Other io) = io
printOutput (Error io) = withSGR (SetColor Foreground Vivid Red) io

withIndex :: [String] -> [String]
withIndex = zipWith (\i e -> show i ++ ": " ++ e) [1..]

withSGR :: SGR -> IO () -> IO ()
withSGR sgr io = do
  setSGR [sgr]
  io
  setSGR [Reset]
