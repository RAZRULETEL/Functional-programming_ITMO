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

singleton :: forall a b. a -> b -> Dict a b
singleton key value = CreateDict ({ root: Just $ singletonNode key value Nothing Nothing })

singletonNode :: forall a b. a -> b -> Maybe (DictNode a b) -> Maybe (DictNode a b) -> DictNode a b
singletonNode key value leftLeaf rightLeaf = CreateDictNode ({ key: key, value: value, leftLeaf: leftLeaf, rightLeaf: rightLeaf })

insert :: forall a b. Ord a => Dict a b -> a -> b -> Dict a b
insert (CreateDict dict) key value = CreateDict ({ root: Just $ insertInternal dict.root })
  where
  insertInternal :: Ord a => Maybe (DictNode a b) -> DictNode a b
  insertInternal Nothing = singletonNode key value Nothing Nothing
  insertInternal (Just (CreateDictNode node))
    | key > node.key = singletonNode node.key node.value node.leftLeaf $ Just $ insertInternal node.rightLeaf
    | key < node.key = singletonNode node.key node.value (Just $ insertInternal node.leftLeaf) node.rightLeaf
    | otherwise = (CreateDictNode node) -- duplicates not allowed

remove :: forall a b. Ord a => Dict a b -> a -> Dict a b
remove (CreateDict dict) key = CreateDict ({ root: removeInternal dict.root })
  where
  removeInternal :: Ord a => Maybe (DictNode a b) -> Maybe (DictNode a b)
  removeInternal Nothing = Nothing -- nothing to do with empty dict
  removeInternal (Just (CreateDictNode node))
    | key > node.key = Just $ singletonNode node.key node.value node.leftLeaf $ removeInternal node.rightLeaf
    | key < node.key = Just $ singletonNode node.key node.value (removeInternal node.leftLeaf) node.rightLeaf
    | otherwise = Nothing -- remove on find

leftTurn :: forall a b. Dict a b -> Dict a b
leftTurn (CreateDict { root: Nothing }) = (CreateDict { root: Nothing })
leftTurn (CreateDict { root: node }) = (CreateDict { root: Just $ leftTurnInternal node })
  where
  leftTurnInternal :: DictNode a b -> DictNode a b
  leftTurnInternal (CreateDictNode root) = case root.rightLeaf of
    Nothing -> (CreateDictNode root)
    Just (CreateDictNode right) -> singletonNode
      right.key
      right.value
      ( Just $ singletonNode
          root.key
          root.value
          root.leftLeaf
          right.leftLeaf
      )
      right.rightLeaf

rightTurn :: forall a b. Dict a b -> Dict a b
rightTurn (CreateDict { root: Nothing }) = (CreateDict { root: Nothing })
rightTurn (CreateDict { root: node }) = (CreateDict { root: Just $ rightTurnInternal node })
  where
  rightTurnInternal :: DictNode a b -> DictNode a b
  rightTurnInternal (CreateDictNode root) = case root.leftLeaf of
    Nothing -> (CreateDictNode root)
    Just (CreateDictNode left) -> singletonNode
      left.key
      left.value
      left.leftLeaf
      ( Just $ singletonNode
          root.key
          root.value
          left.rightLeaf
          root.rightLeaf
      )