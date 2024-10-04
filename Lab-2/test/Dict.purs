module Test.Dict where

import Prelude
import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Dict (get, height, insert, length, remove, singleton)
import Data.Maybe (Maybe(Just), Maybe(Nothing))

emptyDict = remove (singleton 1 5) 1
soloDict = singleton 1 5
simpleRotatedDict = insert (insert (insert (singleton 1 2) 3 4) 5 6) 7 8
difficultRotatedDict = insert (insert (insert (insert (singleton 7 8) 5 6) 9 10) 1 2) 3 4

testDict :: Effect Unit
testDict =
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