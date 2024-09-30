module Test.Main where

import Prelude
import Effect (Effect)
import Test.Numbers (testNumbers)
import Test.Triangle (testTriangle)

main :: Effect Unit
main = do
  testNumbers
  testTriangle