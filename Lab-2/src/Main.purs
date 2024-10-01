module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Dict (Dict(..), DictNode(..), length)
import Data.Show (show)
import Data.Maybe (Maybe(Nothing), Maybe(Just))

empty :: Dict Int
empty = CreateDict ({ root: Nothing })

nonEmpty :: Dict Int
nonEmpty = CreateDict ({ root: Just zero })
  where
  zero = CreateDictNode ({ key: 0, value: 0, leftLeaf: Nothing, rightLeaf: Nothing })

multiLayer :: Dict Int
multiLayer = CreateDict ({ root: Just first })
  where
  third = CreateDictNode ({ key: 0, value: 0, leftLeaf: Nothing, rightLeaf: Nothing })
  second = CreateDictNode ({ key: 0, value: 0, leftLeaf: Nothing, rightLeaf: Just third })
  first = CreateDictNode ({ key: 0, value: 0, leftLeaf: Just second, rightLeaf: Nothing })

main :: Effect Unit
main = do
  log $ show $ length empty
  log $ show $ length nonEmpty
  log $ show $ length multiLayer
