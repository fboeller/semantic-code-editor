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
      \x -> null $ subForest $ levelFilteredTree [pure False] x

    it "results in the same tree for a single true predicate" $ treeProperty $
      \x -> levelFilteredTree [pure True] x == x

    it "results in an empty tree for infinitely many false predicates" $ treeProperty $
      \x -> null $ subForest $ levelFilteredTree (repeat $ pure False) x

    it "results in the same tree for infinitely many true predicates" $ treeProperty $
      \x -> levelFilteredTree (repeat $ pure True) x == x

    it "has maximum height 3 for a false third predicate" $ treeProperty $
      \x -> height (levelFilteredTree [pure True, pure True, pure False] x) <= 3

  describe "cutLateAndEarlyLeafs" $ do
    
    it "results in an empty tree for a desired height of 0" $ treeProperty $
      \x -> null $ subForest $ cutLateAndEarlyLeafs 0 x

    it "results in an empty tree for a desired height greater than the height of the tree" $ treeProperty $
      \x -> null $ subForest $ cutLateAndEarlyLeafs (1 + height x) x

    it "only contains branches of maximum the desired height if the height is within the tree height" $ property $
      \x -> forAll (elements [1..height x]) $
            \h -> height (cutLateAndEarlyLeafs h (x::Tree ())) <= h + 1

  describe "selectedBranches" $ do
    
    it "results in an empty tree for no predicates" $ treeProperty $
      \x -> null $ subForest $ selectedBranches [] x

    it "results in an empty tree for a single false predicate" $ treeProperty $
      \x -> null $ subForest $ selectedBranches [pure False] x

    it "results in an empty tree for more true predicates than the height is" $ treeProperty $
      \x -> null $ subForest $ selectedBranches (replicate (height x) $ pure True) x

    it "results in a tree with the same height for as many true predicates as the height minus the root is" $ treeProperty $
      \x -> height (selectedBranches (replicate (height x - 1) $ pure True) x) == height x

    it "results in a tree with height n+1 for n true predicates if n is smaller than the height of the tree" $ property $
      \x -> forAll (elements [0..height x - 1]) $
            \n -> height (selectedBranches (replicate n $ pure True) (x::Tree ())) == n + 1

