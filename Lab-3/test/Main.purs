module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal) as Assert
import Interpolation (linearInterpolate, newtonInterpolate)
import Data.List (fromFoldable, foldl) as List
import Data.Tuple (Tuple(Tuple))
import Data.Monoid (mempty) as List
import Data.Number (sin)
import Data.List (foldl)

main :: Effect Unit
main = do
  runTest do
    test "Conveyor interpolation" do
      Assert.equal true true
    suite "Linear interpolation" do
      test "[]"
        $ Assert.equal List.mempty
        $ linearInterpolate (List.fromFoldable []) 1.0
      test "[1 2, 3 4]"
        $ Assert.equal (List.fromFoldable [ Tuple 1.0 2.0, Tuple 2.0 3.0, Tuple 3.0 4.0 ])
        $ linearInterpolate (List.fromFoldable [ Tuple 1.0 2.0, Tuple 3.0 4.0 ]) 1.0
      test "[1 10000, 3 2, 9 100]"
        $ Assert.equal (List.fromFoldable [ Tuple 2.0 (-10.0), Tuple 3.0 (-7.5), Tuple 4.0 (-5.0), Tuple 5.0 (-2.5), Tuple 6.0 0.0, Tuple 7.0 2.5, Tuple 8.0 5.0, Tuple 9.0 7.5, Tuple 10.0 10.0 ])
        $ linearInterpolate (List.fromFoldable [ Tuple 1.0 10000.0, Tuple 2.0 (-10.0), Tuple 10.0 10.0 ]) 1.0
    suite "Newton interpolation" do
      test "[]"
        $ Assert.equal List.mempty
        $ newtonInterpolate (List.fromFoldable []) 1.0
      test "[1 2, 3 4]"
        $ Assert.equal List.mempty
        $ newtonInterpolate (List.fromFoldable [ Tuple 1.0 2.0, Tuple 3.0 4.0 ]) 1.0
      test "[1 2, 3 4, 5 6, 7 8]"
        $ Assert.equal
            ( List.fromFoldable
                [ Tuple 1.0 2.0
                , Tuple 2.0 3.0
                , Tuple 3.0 4.0
                , Tuple 4.0 5.0
                , Tuple 5.0 6.0
                , Tuple 6.0 7.0
                , Tuple 7.0 8.0
                , Tuple 8.0 9.000000000000002
                , Tuple 9.0 10.000000000000004
                ]
            )
        $ newtonInterpolate (List.fromFoldable [ Tuple 1.0 2.0, Tuple 3.0 4.0, Tuple 5.0 6.0, Tuple 7.0 8.0, Tuple 9.0 10.0 ]) 1.0
      test "[0 0, 0.5 0.47943, 1.5 0.99749, 3 0.14112, 3.14 0] (sin)"
        $ Assert.equal true
        $ foldl
            (\acc (Tuple _ y) -> acc && (y - sin(y)) < 0.0001)
            true
            ( map
                (\(Tuple x y) -> Tuple x $ y - sin (x))
                $ newtonInterpolate
                    ( List.fromFoldable
                        [ Tuple 0.0 0.0
                        , Tuple 0.5 $ sin (0.5)
                        , Tuple 1.5 $ sin (1.5)
                        , Tuple 3.0 $ sin (3.0)
                        , Tuple 3.14 0.0
                        ]
                    )
                    0.1
            )