module Test.Dict where

import Prelude
import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Dict (get, height, insert, length, remove, singleton)
import Data.Maybe (Maybe(Just), Maybe(Nothing))
import Data.Tuple (Tuple(Tuple))
import Data.Show (show)
import Effect.Console (log)

emptyDict = remove (singleton 1 5) 1
soloDict = singleton 1 5
simpleRotatedDict = insert (insert (insert (singleton 1 2) 3 4) 5 6) 7 8
difficultRotatedDict = insert (insert (insert (insert (singleton 7 8) 5 6) 9 10) 1 2) 3 4

simpleUnbalancedRemoveDict = remove simpleRotatedDict 3
simpleBalancedRemoveDict = remove difficultRotatedDict 3
difficultSubTreeRemoveDict = remove difficultRotatedDict 7
difficultRightSubTreeRemoveDict = remove (insert (insert (insert (insert (singleton 7 8) 5 6) 11 12) 13 14) 9 10) 7


testDict :: Effect Unit
testDict = do
  (log $ show difficultRightSubTreeRemoveDict)
  runTest do
    test "AVL dict" do
      Assert.equal true true
    suite "length" do
      test "0"
        $ Assert.equal 0
        $ length emptyDict
      test "1"
        $ Assert.equal 1
        $ length soloDict
      test "4"
        $ Assert.equal 4
        $ length simpleRotatedDict
      test "5"
        $ Assert.equal 5
        $ length difficultRotatedDict
    suite "height" do
      test "0"
        $ Assert.equal 0
        $ height emptyDict
      test "1"
        $ Assert.equal 1
        $ height soloDict
      test "3"
        $ Assert.equal 3
        $ height simpleRotatedDict
      test "3"
        $ Assert.equal 3
        $ height difficultRotatedDict
    suite "get" do
      test "from empty"
        $ Assert.equal Nothing
        $ get emptyDict 1
      test "from solo"
        $ Assert.equal (Just 5)
        $ get soloDict 1
      test "from simple rotated"
        $ Assert.equal (Just 2)
        $ get simpleRotatedDict 1
      test "from difficult rotated"
        $ Assert.equal (Just 2)
        $ get simpleRotatedDict 1
    suite "remove" do
      test "from solo"
        $ Assert.equal Nothing
        $ get emptyDict 1
      test "simple search: balanced"
        $ Assert.equal (Tuple (Just 2) (Just 6))
        $ Tuple (get simpleBalancedRemoveDict 1) (get simpleBalancedRemoveDict 5)
      test "simple search: unbalanced"
        $ Assert.equal (Tuple (Just 2) (Just 8))
        $ Tuple (get simpleUnbalancedRemoveDict 1) (get simpleUnbalancedRemoveDict 7)
      test "difficult search: balanced subtrees or left > right"
        $ Assert.equal [Nothing, (Just 6), (Just 10), (Just 2)]
        $ [(get difficultSubTreeRemoveDict 7), (get difficultSubTreeRemoveDict 5),
            (get difficultSubTreeRemoveDict 9), (get difficultSubTreeRemoveDict 1)]
      test "difficult search: ubbalanced subtrees (right > left)"
        $ Assert.equal [Nothing, (Just 14), (Just 6), (Just 10)]
        $ [(get difficultRightSubTreeRemoveDict 7), (get difficultRightSubTreeRemoveDict 13),
            (get difficultRightSubTreeRemoveDict 5), (get difficultRightSubTreeRemoveDict 9)]