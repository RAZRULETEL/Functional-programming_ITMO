module Test.Dict where

import Prelude hiding (map)
import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Dict (Dict, filter, foldlDict, foldrDict, get, height, insert, length, map, remove, singleton)
import Data.Maybe (Maybe(Just), Maybe(Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Show (show)
import Test.QuickCheck (quickCheck)
import Data.Monoid (mempty)
import Data.Array (foldl)

emptyDict = remove (singleton 1 5) 1
soloDict = singleton 1 5
simpleRotatedDict = insert (insert (insert (singleton 1 2) 3 4) 5 6) 7 8
difficultRotatedDict = insert (insert (insert (insert (singleton 7 8) 5 6) 9 10) 1 2) 3 4
difficultRotatedDictMult3 = insert (insert (insert (insert (singleton 21 24) 15 18) 27 30) 3 6) 9 12

simpleUnbalancedRemoveDict = remove simpleRotatedDict 3
simpleBalancedRemoveDict = remove difficultRotatedDict 3
difficultSubTreeRemoveDict = remove difficultRotatedDict 7
difficultRightSubTreeRemoveDict = remove (insert (insert (insert (insert (singleton 7 8) 5 6) 11 12) 13 14) 9 10) 7

balanceRRemoveDict = remove (remove difficultRotatedDict 5) 9
balanceLRRemoveDict = remove difficultRotatedDict 9

testDict :: Effect Unit
testDict = do
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
        $ Assert.equal [ Nothing, (Just 6), (Just 10), (Just 2) ]
        $
          [ (get difficultSubTreeRemoveDict 7)
          , (get difficultSubTreeRemoveDict 5)
          , (get difficultSubTreeRemoveDict 9)
          , (get difficultSubTreeRemoveDict 1)
          ]
      test "difficult search: unbalanced subtrees (right > left)"
        $ Assert.equal [ Nothing, (Just 14), (Just 6), (Just 10) ]
        $
          [ (get difficultRightSubTreeRemoveDict 7)
          , (get difficultRightSubTreeRemoveDict 13)
          , (get difficultRightSubTreeRemoveDict 5)
          , (get difficultRightSubTreeRemoveDict 9)
          ]
      test "balance: rigth rotation"
        $ Assert.equal [ Nothing, (Just 8), (Just 2), (Just 4) ]
        $
          [ (get balanceRRemoveDict 9)
          , (get balanceRRemoveDict 7)
          , (get balanceRRemoveDict 1)
          , (get balanceRRemoveDict 3)
          ]
      test "balance: left-rigth rotation"
        $ Assert.equal [ Nothing, (Just 6), (Just 2), (Just 4) ]
        $
          [ (get balanceLRRemoveDict 9)
          , (get balanceLRRemoveDict 5)
          , (get balanceLRRemoveDict 1)
          , (get balanceLRRemoveDict 3)
          ]
    suite "filter" do
      test "match all"
        $ Assert.equal difficultRotatedDict
        $ filter (\key value -> key == value) difficultRotatedDict
      test "no matches"
        $ Assert.equal emptyDict
        $ filter (\key value -> not (key == value)) difficultRotatedDict
    suite "map" do
      test "multiply by 3 (assert keys)"
        $ Assert.equal (Tuple emptyDict emptyDict)
        $ do
            let multiplied = map (\key value -> Tuple (key * 3) (value * 3)) difficultRotatedDict
            let getOrZero key dict = fromMaybe 0 $ get dict key
            Tuple
              (filter (\key value -> not (getOrZero key difficultRotatedDictMult3 == 0)) multiplied)
              (filter (\key value -> not (getOrZero key multiplied == 0)) difficultRotatedDictMult3)
      test "multiply by 3 (assert values)"
        $ Assert.equal difficultRotatedDictMult3
        $ map (\key value -> Tuple (key * 3) (value * 3)) difficultRotatedDict
    suite "foldr" do
      test "to ascending string"
        $ Assert.equal "1 2 3 4 5 6 7 8 "
        $ foldrDict (\key value str -> show key <> " " <> show value <> " " <> str) "" simpleRotatedDict
      test "to descending string"
        $ Assert.equal " 7 8 5 6 3 4 1 2"
        $ foldrDict (\key value str -> str <> " " <> show key <> " " <> show value) "" simpleRotatedDict
      test "substraction"
        $ Assert.equal (2 - (4 - (6 - (8 - 0))))
        $ foldrDict (\key value acc -> value - acc) 0 simpleRotatedDict
    suite "foldl" do
      test "to ascending string"
        $ Assert.equal " 1 2 3 4 5 6 7 8"
        $ foldlDict (\key str value -> str <> " " <> show key <> " " <> show value) "" simpleRotatedDict
      test "to descending string"
        $ Assert.equal "7 8 5 6 3 4 1 2 "
        $ foldlDict (\key str value -> show key <> " " <> show value <> " " <> str) "" simpleRotatedDict
      test "substraction"
        $ Assert.equal ((((0 - 2) - 4) - 6) - 8)
        $ foldlDict (\key acc value -> acc - value) 0 simpleRotatedDict
  quickCheck testableDict
  where
  generatedDict :: forall a b. Ord a => Array (Tuple a b) -> Dict a b
  generatedDict arr = foldl (\dict tuple -> uncurry (\k v -> insert dict k v) tuple) mempty arr

  testableDict :: Array (Tuple Int Int) -> Boolean
  testableDict arr = do
    let dict = generatedDict arr
    eq (append mempty (append mempty dict)) dict

