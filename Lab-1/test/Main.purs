module Test.Main where

import Prelude
import Effect (Effect)
import Test.Numbers (testNumbers)


main :: Effect Unit
main = do
    testNumbers