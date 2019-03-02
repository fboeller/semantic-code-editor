module TreesSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Containers

import Data.Tree

import Trees

spec = describe "Trees" $ do

  describe "levelFilteredTree" $ do
  
    it "results in the same tree for no predicates" $ property $
      \x -> levelFilteredTree [] x == (x::Tree Int)

    it "results in an empty tree for a single false predicate" $ property $
      \x -> (subForest $ levelFilteredTree [pure False] (x::Tree Int)) == []

    it "results in the same tree for a single true predicate" $ property $
      \x -> levelFilteredTree [pure True] (x::Tree Int) == x

    it "results in an empty tree for infinitely many false predicates" $ property $
      \x -> (subForest $ levelFilteredTree (repeat $ pure False) (x::Tree Int)) == []

    it "results in the same tree for infinitely many true predicates" $ property $
      \x -> levelFilteredTree (repeat $ pure True) (x::Tree Int) == x

    it "has maximum height 3 for a false third predicate" $ property $
      \x -> (length $ levels $ levelFilteredTree [pure True, pure True, pure False] (x::Tree Int)) <= 3
