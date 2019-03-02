module Trees where

import Control.Lens hiding (elements)

import Data.Tree

-- A tree of all elements beneath this element to which the given predicates apply.
-- Elements are only included in the resulting tree iff...
-- ... they satisfy the predicate of their level.
-- ... all their parents satisfy the predicate of their level.
-- ... there is a predicate for their level.
selectedBranches :: [a -> Bool] -> Tree a -> Tree a
selectedBranches predicates = cutLateAndEarlyLeafs (length predicates) . levelFilteredTree predicates

-- Cuts all branches from the tree that do not end on the given level
cutLateAndEarlyLeafs :: Int -> Tree a -> Tree a
cutLateAndEarlyLeafs level = cutLateLeafs level . cutEarlyLeafs level

-- Cuts all leafs from the tree that are on a level smaller than the given number.
cutEarlyLeafs :: Int -> Tree a -> Tree a
cutEarlyLeafs 1 tree = tree
cutEarlyLeafs level (Node label subForest) =
  subForest
  & map (cutEarlyLeafs (level - 1))
  & filter (not.isLeaf)
  & Node label

-- Taken from Hledger.Utils.Tree
-- Cuts all elements from the tree that are on a level greater than the given number.
cutLateLeafs :: Int -> Tree a -> Tree a
cutLateLeafs 0 t = Node (rootLabel t) []
cutLateLeafs d t = Node (rootLabel t) $ cutLateLeafs (d-1) <$> subForest t

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

treefilter :: (a -> Bool) -> Tree a -> Tree a
treefilter f t = Node
                 (rootLabel t)
                 (map (treefilter f) $ filter (treeany f) $ subForest t)

treeany :: (a -> Bool) -> Tree a -> Bool
treeany f t = f (rootLabel t) || any (treeany f) (subForest t)
