module Trees where

import Data.Tree
import qualified Data.List as L
import Data.Function (on)
import Data.Ord (comparing)

-- A tree of all elements beneath this element to which the given predicates apply.
-- Elements are only included in the resulting tree iff...
-- ... there is a predicate for their level.
-- ... they satisfy the predicate of their level.
-- ... all their parents satisfy the predicate of their level.
selectedBranches :: [a -> Bool] -> Tree a -> Tree a
selectedBranches predicates =
  cutLateAndEarlyLeafs (length predicates) . levelFilteredTree predicates

-- Finds the element following the given path through the tree
elementAtIndex :: [Int] -> Tree a -> Maybe (Tree a)
elementAtIndex [] tree = Just tree
elementAtIndex (index:restPath) (Node _ elements) =
  findOrElse index elements (elementAtIndex restPath) Nothing

-- Finds the element at the given index in the list and returns the transformation according to the given function
-- or returns the default if the index is out of bounds
findOrElse :: Int -> [a] -> (a -> b) -> b -> b
findOrElse index list f d =
  if index > 0 && index <= length list then
    f $ list !! (index - 1)
  else
    d

-- Cuts all branches from the tree that do not end on the given level
-- The root node has level 1.
cutLateAndEarlyLeafs :: Int -> Tree a -> Tree a
cutLateAndEarlyLeafs level =
  cutLateLeafs level . cutEarlyLeafs level

-- Cuts all leafs from the tree that are on a level smaller than the given number.
cutEarlyLeafs :: Int -> Tree a -> Tree a
cutEarlyLeafs 1 tree = tree
cutEarlyLeafs level tree =
  Node (rootLabel tree) $ filter (not.isLeaf) $ cutEarlyLeafs (level - 1) <$> subForest tree

-- Cuts all elements from the tree that are on a level greater than the given number.
cutLateLeafs :: Int -> Tree a -> Tree a
cutLateLeafs 0 tree = Node (rootLabel tree) []
cutLateLeafs level tree =
  Node (rootLabel tree) $ cutLateLeafs (level - 1) <$> subForest tree

isLeaf :: Tree a -> Bool
isLeaf = null . subForest

height :: Tree a -> Int
height = length . levels

sortOn :: (Ord b) => (a -> b) -> Tree a -> Tree a
sortOn = sortBy . comparing

sortBy :: (a -> a -> Ordering) -> Tree a -> Tree a
sortBy compare = foldTree (\label -> Node label . L.sortBy (compare `on` rootLabel))

-- Removes all elements from the tree that do not satisfy the given predicate for the level they are on.
-- The given list defines the predicates where the nth predicate corresponds to the nth level of the tree.
-- If there are more levels in the tree than predicates in the list, then elements on deeper levels won't be removed.
levelFilteredTree :: [a -> Bool] -> Tree a -> Tree a
levelFilteredTree [] tree = tree
levelFilteredTree (p:ps) tree =
  Node (rootLabel tree) $ levelFilteredTree ps <$> filter (p.rootLabel) (subForest tree)

-- Unfolds a possibly infinite tree with the given function.
recursively :: (a -> [a]) -> a -> Tree a
recursively f = unfoldTree (\b -> (b, f b))

-- Taken from Hledger.Utils.Tree

treefilter :: (a -> Bool) -> Tree a -> Tree a
treefilter f t =
  Node (rootLabel t) $ treefilter f <$> filter (treeany f) (subForest t)

treeany :: (a -> Bool) -> Tree a -> Bool
treeany f t = f (rootLabel t) || any (treeany f) (subForest t)
