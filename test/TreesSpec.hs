module TreesSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Containers

import Data.Tree

import Trees

treeProperty :: (Tree () -> Bool) -> Property
treeProperty = property

spec = describe "Trees" $ do

  describe "levelFilteredTree" $ do
  
    it "results in the same tree for no predicates" $ treeProperty $
      \x -> levelFilteredTree [] x == x

    it "results in an empty tree for a single false predicate" $ treeProperty $
      \x -> (subForest $ levelFilteredTree [pure False] x) == []

    it "results in the same tree for a single true predicate" $ treeProperty $
      \x -> levelFilteredTree [pure True] x == x

    it "results in an empty tree for infinitely many false predicates" $ treeProperty $
      \x -> (subForest $ levelFilteredTree (repeat $ pure False) x) == []

    it "results in the same tree for infinitely many true predicates" $ treeProperty $
      \x -> levelFilteredTree (repeat $ pure True) x == x

    it "has maximum height 3 for a false third predicate" $ treeProperty $
      \x -> (height $ levelFilteredTree [pure True, pure True, pure False] x) <= 3

  describe "cutLateAndEarlyLeafs" $ do
    
    it "results in an empty tree for a desired height of 0" $ treeProperty $
      \x -> (subForest $ cutLateAndEarlyLeafs 0 x) == []

    it "results in an empty tree for a desired height greater than the height of the tree" $ treeProperty $
      \x -> (subForest $ cutLateAndEarlyLeafs (1 + height x) x) == []

    it "only contains branches of maximum the desired height if the height is within the tree height" $ property $
      \x -> forAll (elements [1..height x]) $
            \h -> (height $ cutLateAndEarlyLeafs h (x::Tree ())) <= h + 1
