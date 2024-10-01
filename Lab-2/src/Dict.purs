module Dict where

import Prelude

import Data.Maybe (Maybe, Maybe(Nothing), Maybe(..))
import Data.Boolean (otherwise)

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

insert :: forall a b. Ord a => Dict a b -> a -> b -> Dict a b
insert (CreateDict dict) key value = CreateDict ({root: Just $ insertInternal dict.root})
  where
  insertInternal :: Ord a => Maybe (DictNode a b) -> DictNode a b
  insertInternal Nothing = CreateDictNode({ key: key, value: value, leftLeaf: Nothing, rightLeaf: Nothing })
  insertInternal (Just (CreateDictNode node)) | key > node.key = CreateDictNode({ key: node.key, value: node.value, leftLeaf: node.leftLeaf, rightLeaf: Just $ insertInternal node.rightLeaf })
                                                        | key < node.key = CreateDictNode({ key: node.key, value: node.value, leftLeaf: Just $ insertInternal node.leftLeaf, rightLeaf: node.rightLeaf })
                                                        | otherwise = (CreateDictNode node) -- duplicates not allowed

singleton :: forall a b. a -> b -> Dict a b
singleton key value = CreateDict ({root: Just $ singletonNode key value})

singletonNode :: forall a b. a -> b -> DictNode a b
singletonNode key value = CreateDictNode({ key: key, value: value, leftLeaf: Nothing, rightLeaf: Nothing })