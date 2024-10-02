module Test.Main where

import Prelude

import Effect (Effect)
import Test.Dict (testDict)

main :: Effect Unit
main = do
  testDict
