module Dict where

import Prelude

import Data.Maybe (Maybe, isNothing)

newtype DictNode a b = CreateDictNode
  { key :: a
  , value :: b
  , leftLeaf :: Maybe (DictNode a b)
  , rightLeaf :: Maybe (DictNode a b)
  }

newtype Dict a = CreateDict
  { root :: Maybe (DictNode a a)
  }

length :: forall a. Dict a -> Int
length (CreateDict dict) = if (isNothing dict.root) then 0 else 1
