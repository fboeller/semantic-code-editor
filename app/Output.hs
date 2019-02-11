module Output where

import qualified Java as J
import PromptShow

import Control.Lens ((^.))
import Data.Tree
import System.Console.ANSI

data Output =
  ResultTree (Tree J.Element) |
  Other (IO ()) |
  Error (IO ())
  
printOutput :: Output -> IO ()
printOutput (ResultTree elements) = putStr $ printResultTree 0 1 $ printSignature <$> elements
printOutput (Other io) = io
printOutput (Error io) = withSGR (SetColor Foreground Vivid Red) io

printResultTree :: Int -> Int -> Tree String -> String
printResultTree 0 _ (Node _ subs) = printResultForest 0 subs
printResultTree level index (Node label subs) = concat
  [ indent ++ show index ++ ": " ++ label ++ "\n"
  , printResultForest level subs
  ]
  where indent = replicate ((level-1)*2) ' '

printResultForest :: Int -> Forest String -> String
printResultForest level subs =
  concat $ (uncurry $ printResultTree $ level+1) <$> zip [1..] subs

withIndex :: [String] -> [String]
withIndex = zipWith (\i e -> show i ++ ": " ++ e) [1..]

withSGR :: SGR -> IO () -> IO ()
withSGR sgr io = do
  setSGR [sgr]
  io
  setSGR [Reset]
