module Test.Dict where

import Prelude
import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Dict (balanceTree, get, height, insert, length, remove, singleton)
import Data.Maybe (Maybe(Just))

testDict :: Effect Unit
testDict =
  runTest do
    test "AVL dict" do
      Assert.equal true true
    suite "length" do
      test "0"
        $ Assert.equal 0
        $ length $ remove (singleton 1 5) 1
      test "1"
        $ Assert.equal 1
        $ length (singleton 1 5)
      test "4"
        $ Assert.equal 4
        $ length $ (insert (insert (insert (singleton 1 2) 3 4) 5 6) 7 8)
    suite "height" do
      test "0"
        $ Assert.equal 0
        $ height $ remove (singleton 1 5) 1
      test "1"
        $ Assert.equal 1
        $ height (singleton 1 5)
      test "4 (not balanced)" -- TODO: remove when auto balancing implemented
        $ Assert.equal 4
        $ height $ (insert (insert (insert (singleton 1 2) 3 4) 5 6) 7 8)
      test "4 (balanced to 3)"
        $ Assert.equal 3
        $ height $ balanceTree (insert (insert (insert (singleton 1 2) 3 4) 5 6) 7 8)
    suite "get" do
      test "from root"
        $ Assert.equal (Just 5)
        $ get (singleton 1 5) 1
      test "from leaf"
        $ Assert.equal (Just 4)
        $ get (insert (singleton 1 2) 3 4) 3