module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Dict (Dict(..), DictNode(..), get, insert, length, singleton)
import Data.Show (show)
import Data.Maybe (Maybe(Nothing), Maybe(Just))

empty :: Dict Int Int
empty = CreateDict ({ root: Nothing })

nonEmpty :: Dict Int Int
nonEmpty = CreateDict ({ root: Just zero })
  where
  zero = CreateDictNode ({ key: 0, value: 0, leftLeaf: Nothing, rightLeaf: Nothing, height: 0 })

multiLayer :: Dict Int Int
multiLayer = CreateDict ({ root: Just first })
  where
  third = CreateDictNode ({ key: 0, value: 0, leftLeaf: Nothing, rightLeaf: Nothing, height: 0 })
  second = CreateDictNode ({ key: 0, value: 0, leftLeaf: Nothing, rightLeaf: Just third, height: 0 })
  first = CreateDictNode ({ key: 0, value: 0, leftLeaf: Just second, rightLeaf: Nothing, height: 0 })

insertEls :: Dict Int Int
insertEls = insert (insert (singleton 1 1) 2 2) 1 1

main :: Effect Unit
main = do
  log $ show $ length empty
  log $ show $ length nonEmpty
  log $ show $ length multiLayer
  log $ show $ length insertEls
  log $ show $ get insertEls 2