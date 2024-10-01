module Dict where

import Prelude

import Data.Maybe (Maybe, Maybe(Nothing), Maybe(..))

newtype DictNode a b = CreateDictNode
  { key :: a
  , value :: b
  , leftLeaf :: Maybe (DictNode a b)
  , rightLeaf :: Maybe (DictNode a b)
  }

newtype Dict a b = CreateDict
  { root :: Maybe (DictNode a b)
  }

length :: forall a b. Dict a b -> Int
length (CreateDict dict) = lengthInternal dict.root
  where
  lengthInternal :: Maybe (DictNode a b) -> Int
  lengthInternal maybeNode = case maybeNode of
    Nothing -> 0
    Just (CreateDictNode node) -> 1 + (lengthInternal node.leftLeaf) + (lengthInternal node.rightLeaf)

