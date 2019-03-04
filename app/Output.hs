module Output where

import Java.Types (Element)
import Java.Printer (printMinimal)

import Control.Lens ((^.))
import Data.Tree
import System.Console.ANSI

data Output =
  ResultTree (Tree Element) |
  Other (IO ()) |
  Error (IO ())
  
printOutput :: Output -> IO ()
printOutput (ResultTree elements) = printResultTree 0 1 $ printMinimal <$> elements
printOutput (Other io) = io
printOutput (Error io) = withSGR (SetColor Foreground Vivid Red) io

printResultTree :: Int -> Int -> Tree (IO ()) -> IO ()
printResultTree 0 _ (Node _ subs) = printResultForest 0 subs
printResultTree level index (Node label subs) = mconcat
  [ putStr indent
  , putStr $ show index
  , putStr ": "
  , label
  , putStr "\n"
  , printResultForest level subs
  ]
  where indent = replicate ((level-1)*2) ' '

printResultForest :: Int -> Forest (IO ()) -> IO ()
printResultForest level subs =
  mconcat $ uncurry (printResultTree $ level+1) <$> zip [1..] subs

withIndex :: [String] -> [String]
withIndex = zipWith (\i e -> show i ++ ": " ++ e) [1..]

withSGR :: SGR -> IO () -> IO ()
withSGR sgr io = do
  setSGR [sgr]
  io
  setSGR [Reset]
