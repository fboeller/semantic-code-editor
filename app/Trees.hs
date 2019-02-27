module Trees where

import Control.Lens hiding (elements)

import Data.Tree

-- Cuts all elements from the tree that are on a level greater than the given number.
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

-- Removes all elements from the tree that do not satisfy the given predicate for the level they are on.
-- The given list defines the predicates where the nth predicate corresponds to the nth level of the tree.
-- If there are more levels in the tree than predicates in the list, then elements on deeper levels won't be removed.
levelFilteredTree :: [a -> Bool] -> Tree a -> Tree a
levelFilteredTree [] node = node
levelFilteredTree (p:ps) (Node label subForest) =
  subForest
  & filter (\(Node l _) -> p l)
  & map (levelFilteredTree ps)
  & Node label

-- Unfolds a possibly infinite tree with the given function.
recursively :: (a -> [a]) -> a -> Tree a
recursively f = unfoldTree (\b -> (b, f b))

-- Taken from Hledger.Utils.Tree

treeprune :: Int -> Tree a -> Tree a
treeprune 0 t = Node (rootLabel t) []
treeprune d t = Node (rootLabel t) $ treeprune (d-1) <$> subForest t

treefilter :: (a -> Bool) -> Tree a -> Tree a
treefilter f t = Node
                 (rootLabel t)
                 (map (treefilter f) $ filter (treeany f) $ subForest t)

treeany :: (a -> Bool) -> Tree a -> Bool
treeany f t = f (rootLabel t) || any (treeany f) (subForest t)
