module Trees where

import Control.Lens hiding (elements)

import Data.Tree

cutEarlyLeafs :: Int -> Tree a -> Tree a
cutEarlyLeafs 1 tree = tree
cutEarlyLeafs level (Node label subForest) =
  subForest
  & map (cutEarlyLeafs (level - 1))
  & filter (not.isLeaf)
  & Node label

isLeaf :: Tree a -> Bool
isLeaf (Node _ []) = True
isLeaf _ = False

levelFilteredTree :: [a -> Bool] -> Tree a -> Tree a
levelFilteredTree [] node = node
levelFilteredTree (p:ps) (Node label subForest) =
  subForest
  & filter (\(Node l _) -> p l)
  & map (levelFilteredTree ps)
  & Node label
    
-- Taken from Hledger.Utils.Tree

treeprune :: Int -> Tree a -> Tree a
treeprune 0 t = Node (rootLabel t) []
treeprune d t = Node (rootLabel t) $ (treeprune $ d-1) <$> subForest t

treefilter :: (a -> Bool) -> Tree a -> Tree a
treefilter f t = Node
                 (rootLabel t)
                 (map (treefilter f) $ filter (treeany f) $ subForest t)

treeany :: (a -> Bool) -> Tree a -> Bool
treeany f t = f (rootLabel t) || any (treeany f) (subForest t)


