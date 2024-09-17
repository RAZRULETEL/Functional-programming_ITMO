module Test.Triangle where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal) as Assert
import Triangle (recursiveTriangle)


testTriangle :: Effect Unit
testTriangle =
    runTest do
        test "Triangle (Euler 18)" do
            Assert.equal true true
        suite "recursiveTriangle" do
            test "One number pyramid"
                $ Assert.equal 3
                $ recursiveTriangle "3"
            test "Small pyramid"
                $ Assert.equal 4
                $ recursiveTriangle "1\n2 3"
            test "Small pyramid"
                $ Assert.equal 4
                $ recursiveTriangle "1\n2 3"
            test "Big pyramid"
                $ Assert.equal 538
                $ recursiveTriangle """75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67"""

