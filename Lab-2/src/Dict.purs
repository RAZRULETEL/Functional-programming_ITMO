module Dict where

import Prelude

import Data.Maybe (Maybe, Maybe(Nothing), Maybe(..))
import Data.Boolean (otherwise)

newtype DictNode a b = CreateDictNode
  { key :: a
  , value :: b
  , leftLeaf :: Maybe (DictNode a b)
  , rightLeaf :: Maybe (DictNode a b)
  , height :: Int
  }

newtype Dict a b = CreateDict
  { root :: Maybe (DictNode a b)
  }

getHeight :: forall a b. DictNode a b -> Int
getHeight (CreateDictNode node) = node.height

getMaybeHeight :: forall a b. Maybe (DictNode a b)-> Int
getMaybeHeight (Just node) = getHeight node
getMaybeHeight Nothing = -1

length :: forall a b. Dict a b -> Int
length (CreateDict dict) = lengthInternal dict.root
  where
  lengthInternal :: Maybe (DictNode a b) -> Int
  lengthInternal maybeNode = case maybeNode of
    Nothing -> 0
    Just (CreateDictNode node) -> 1 + (lengthInternal node.leftLeaf) + (lengthInternal node.rightLeaf)

singleton :: forall a b. a -> b -> Dict a b
singleton key value = CreateDict ({ root: Just $ singletonNode key value Nothing Nothing 0 })

singletonNode :: forall a b. a -> b -> Maybe (DictNode a b) -> Maybe (DictNode a b) -> Int -> DictNode a b
singletonNode key value leftLeaf rightLeaf height = CreateDictNode ({ key: key, value: value, leftLeaf: leftLeaf, rightLeaf: rightLeaf, height: height })

insert :: forall a b. Ord a => Dict a b -> a -> b -> Dict a b
insert (CreateDict dict) key value = CreateDict ({ root: Just $ insertInternal dict.root })
  where
  insertInternal :: Ord a => Maybe (DictNode a b) -> DictNode a b
  insertInternal Nothing = singletonNode key value Nothing Nothing 0
  insertInternal (Just (CreateDictNode node))
    | key > node.key =
      do
      let editNode = insertInternal node.rightLeaf
      singletonNode node.key node.value node.leftLeaf (Just editNode) $ 1 + max (getHeight editNode) (getMaybeHeight node.leftLeaf)
    | key < node.key =
      do
      let editNode = insertInternal node.rightLeaf
      singletonNode node.key node.value (Just editNode) node.rightLeaf $ 1 + max (getHeight editNode) (getMaybeHeight node.rightLeaf)
    | otherwise = (CreateDictNode node) -- duplicates not allowed

remove :: forall a b. Ord a => Dict a b -> a -> Dict a b
remove (CreateDict dict) key = CreateDict ({ root: removeInternal dict.root })
  where
  removeInternal :: Ord a => Maybe (DictNode a b) -> Maybe (DictNode a b)
  removeInternal Nothing = Nothing -- nothing to do with empty dict
  removeInternal (Just (CreateDictNode node))
    | key > node.key =
      do
      let removedNode = removeInternal node.rightLeaf
      Just $ singletonNode node.key node.value node.leftLeaf removedNode $ 1 + max (getMaybeHeight node.leftLeaf) (getMaybeHeight removedNode)
    | key < node.key =
      do
      let removedNode = removeInternal node.leftLeaf
      Just $ singletonNode node.key node.value removedNode node.rightLeaf $ 1 + max (getMaybeHeight node.rightLeaf) (getMaybeHeight removedNode)
    | otherwise = Nothing -- remove on find

leftTurn :: forall a b. Dict a b -> Dict a b
leftTurn (CreateDict { root: Nothing }) = (CreateDict { root: Nothing })
leftTurn (CreateDict { root: Just node }) = (CreateDict { root: Just $ leftTurnInternal node })
  where
  leftTurnInternal :: DictNode a b -> DictNode a b
  leftTurnInternal (CreateDictNode root) = case root.rightLeaf of
    Nothing -> (CreateDictNode root)
    Just (CreateDictNode right) -> do
      let prevRootHeight = 1 + max (getMaybeHeight root.leftLeaf) (getMaybeHeight right.leftLeaf)
      singletonNode
        right.key
        right.value
        ( Just $ singletonNode
            root.key
            root.value
            root.leftLeaf
            right.leftLeaf
            prevRootHeight
        )
        right.rightLeaf
        (1 + max (getMaybeHeight right.rightLeaf) prevRootHeight)

rightTurn :: forall a b. Dict a b -> Dict a b
rightTurn (CreateDict { root: Nothing }) = (CreateDict { root: Nothing })
rightTurn (CreateDict { root: Just node }) = (CreateDict { root: Just $ rightTurnInternal node })
  where
  rightTurnInternal :: DictNode a b -> DictNode a b
  rightTurnInternal (CreateDictNode root) = case root.leftLeaf of
    Nothing -> (CreateDictNode root)
    Just (CreateDictNode left) -> do
      let prevRootHeight = 1 + max (getMaybeHeight left.rightLeaf) (getMaybeHeight root.rightLeaf)
      singletonNode
        left.key
        left.value
        left.leftLeaf
        ( Just $ singletonNode
            root.key
            root.value
            left.rightLeaf
            root.rightLeaf
            prevRootHeight
        )
        (1 + max (getMaybeHeight left.leftLeaf) prevRootHeight)

get :: forall a b. Ord a => Dict a b -> a -> Maybe b
get (CreateDict dict) key = getInternal dict.root
  where
  getInternal :: Maybe (DictNode a b) -> Maybe b
  getInternal Nothing = Nothing
  getInternal (Just (CreateDictNode node)) | key == node.key = Just node.value
                                           | key > node.key = getInternal node.rightLeaf
                                           | key < node.key = getInternal node.leftLeaf
                                           | otherwise = Nothing
