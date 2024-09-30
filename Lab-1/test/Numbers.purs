module Test.Numbers where

import Prelude

import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Data.Array (singleton, (:))
import Type.Proxy (Proxy(..))
import Data.BigInt (fromTLInt)
import Numbers (sumCase, sumFold, sumGuard, sumInfinite, sumRecursive, sumTailRecursive, textToIntArray)

bigZero = fromTLInt (Proxy :: Proxy 0)
bigOne = fromTLInt (Proxy :: Proxy 1)
bigSeventySeven = fromTLInt (Proxy :: Proxy 77)
bigOneHundredTwentyThree = fromTLInt (Proxy :: Proxy 123)
bigSixHundredFourtyFive = fromTLInt (Proxy :: Proxy 645)

testNumbers :: Effect Unit
testNumbers =
    runTest do
        test "Number (Euler 13)" do
            Assert.equal true true
        suite "textToIntArray" do
            test "Single number"
                $ Assert.equal (singleton bigSeventySeven)
                $ textToIntArray "77"
            test "Three numbers"
                $ Assert.equal (bigSeventySeven : bigOneHundredTwentyThree : bigSixHundredFourtyFive : [])
                $ textToIntArray "77\n123\n645"
            test "Incorrect text"
                $ Assert.equal (singleton bigZero)
                $ textToIntArray "not a number"
        suite "Usual recursion sum" do
            test "1 + 1"
                $ Assert.equal (bigOne + bigOne)
                $ sumRecursive (bigOne : bigOne : [])
            test "77 + 123"
                $ Assert.equal (bigSeventySeven + bigOneHundredTwentyThree)
                $ sumRecursive (bigSeventySeven : bigOneHundredTwentyThree : [])
            test "77 + 123 + 645"
                $ Assert.equal (bigSeventySeven + bigOneHundredTwentyThree + bigSixHundredFourtyFive)
                $ sumRecursive (bigSixHundredFourtyFive : bigSeventySeven : bigOneHundredTwentyThree : [])
            test "645 + 0"
                $ Assert.equal (bigZero + bigSixHundredFourtyFive)
                $ sumRecursive (bigSixHundredFourtyFive : bigZero : [])
        suite "Tail recursion sum" do
            test "1 + 1"
                $ Assert.equal (bigOne + bigOne)
                $ sumTailRecursive (bigOne : bigOne : [])
            test "77 + 123"
                $ Assert.equal (bigSeventySeven + bigOneHundredTwentyThree)
                $ sumTailRecursive (bigSeventySeven : bigOneHundredTwentyThree : [])
            test "77 + 123 + 645"
                $ Assert.equal (bigSeventySeven + bigOneHundredTwentyThree + bigSixHundredFourtyFive)
                $ sumTailRecursive (bigSixHundredFourtyFive : bigSeventySeven : bigOneHundredTwentyThree : [])
            test "645 + 0"
                $ Assert.equal (bigZero + bigSixHundredFourtyFive)
                $ sumTailRecursive (bigSixHundredFourtyFive : bigZero : [])
        suite "Case recursion sum" do
            test "1 + 1"
                $ Assert.equal (bigOne + bigOne)
                $ sumCase (bigOne : bigOne : [])
            test "77 + 123"
                $ Assert.equal (bigSeventySeven + bigOneHundredTwentyThree)
                $ sumCase (bigSeventySeven : bigOneHundredTwentyThree : [])
            test "77 + 123 + 645"
                $ Assert.equal (bigSeventySeven + bigOneHundredTwentyThree + bigSixHundredFourtyFive)
                $ sumCase (bigSixHundredFourtyFive : bigSeventySeven : bigOneHundredTwentyThree : [])
            test "645 + 0"
                $ Assert.equal (bigZero + bigSixHundredFourtyFive)
                $ sumCase (bigSixHundredFourtyFive : bigZero : [])
        suite "Guard recursion sum" do
            test "1 + 1"
                $ Assert.equal (bigOne + bigOne)
                $ sumGuard (bigOne : bigOne : [])
            test "77 + 123"
                $ Assert.equal (bigSeventySeven + bigOneHundredTwentyThree)
                $ sumGuard (bigSeventySeven : bigOneHundredTwentyThree : [])
            test "77 + 123 + 645"
                $ Assert.equal (bigSeventySeven + bigOneHundredTwentyThree + bigSixHundredFourtyFive)
                $ sumGuard (bigSixHundredFourtyFive : bigSeventySeven : bigOneHundredTwentyThree : [])
            test "645 + 0"
                $ Assert.equal (bigZero + bigSixHundredFourtyFive)
                $ sumGuard (bigSixHundredFourtyFive : bigZero : [])
        suite "Fold monolith sum" do
            test "1 + 1"
                $ Assert.equal "2"
                $ sumFold "1\n1"
            test "77 + 123"
                $ Assert.equal "200"
                $ sumFold "77\n123"
            test "77 + 123 + 645"
                $ Assert.equal "845"
                $ sumFold "77\n123\n645"
            test "645 + 0"
                $ Assert.equal "645"
                $ sumFold "645\n0"
            test "Incorrect text"
                $ Assert.equal "0"
                $ sumFold "not a number"
        suite "Infinite list sum" do
            test "1 + 1"
                $ Assert.equal (bigOne + bigOne)
                $ sumInfinite (bigOne : bigOne : [])
            test "77 + 123"
                $ Assert.equal (bigSeventySeven + bigOneHundredTwentyThree)
                $ sumInfinite (bigSeventySeven : bigOneHundredTwentyThree : [])
            test "77 + 123 + 645"
                $ Assert.equal (bigSeventySeven + bigOneHundredTwentyThree + bigSixHundredFourtyFive)
                $ sumInfinite (bigSixHundredFourtyFive : bigSeventySeven : bigOneHundredTwentyThree : [])
            test "645 + 0"
                $ Assert.equal (bigZero + bigSixHundredFourtyFive)
                $ sumInfinite (bigSixHundredFourtyFive : bigZero : [])