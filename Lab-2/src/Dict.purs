module Dict where

import Prelude

import Data.Maybe (Maybe, Maybe(Nothing), Maybe(..))
import Data.Boolean (otherwise)
import Data.Show (show)

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

getMaybeHeight :: forall a b. Maybe (DictNode a b) -> Int
getMaybeHeight (Just node) = getHeight node
getMaybeHeight Nothing = -1

length :: forall a b. Dict a b -> Int
length (CreateDict dict) = lengthInternal dict.root
  where
  lengthInternal :: Maybe (DictNode a b) -> Int
  lengthInternal maybeNode = case maybeNode of
    Nothing -> 0
    Just (CreateDictNode node) -> 1 + (lengthInternal node.leftLeaf) + (lengthInternal node.rightLeaf)

height :: forall a b. Dict a b -> Int
height (CreateDict { root: Nothing }) = 0
height (CreateDict { root: Just (CreateDictNode node) }) = node.height + 1

singleton :: forall a b. a -> b -> Dict a b
singleton key value = CreateDict ({ root: Just $ singletonNode key value Nothing Nothing 0 })

singletonNode :: forall a b. a -> b -> Maybe (DictNode a b) -> Maybe (DictNode a b) -> Int -> DictNode a b
singletonNode key value leftLeaf rightLeaf height = CreateDictNode ({ key: key, value: value, leftLeaf: leftLeaf, rightLeaf: rightLeaf, height: height })

instance (Show a, Show b) => Show (Dict a b) where
  show (CreateDict a) = show a

instance (Show a, Show b) => Show (DictNode a b) where
  show (CreateDictNode node) = "{ key: " <> show node.key <> ", value: " <> "show node.value" <> ", height: " <> show node.height <> ", left: " <> show node.leftLeaf <> " , right: " <> show node.rightLeaf <> " }"

insert :: forall a b. Ord a => Dict a b -> a -> b -> Dict a b
insert (CreateDict dict) key value = CreateDict ({ root: Just $ insertInternal dict.root })
  where
  insertInternal :: Ord a => Maybe (DictNode a b) -> DictNode a b
  insertInternal Nothing = singletonNode key value Nothing Nothing 0
  insertInternal (Just (CreateDictNode node)) = do
    let
      insertedNode
        | key > node.key =
            do
              let editNode = insertInternal node.rightLeaf
              singletonNode node.key node.value node.leftLeaf (Just editNode) $ 1 + max (getHeight editNode) (getMaybeHeight node.leftLeaf)
        | key < node.key =
            do
              let editNode = insertInternal node.leftLeaf
              singletonNode node.key node.value (Just editNode) node.rightLeaf $ 1 + max (getHeight editNode) (getMaybeHeight node.rightLeaf)
        | otherwise = (CreateDictNode node) -- duplicates not allowed
    let insertedRecord = case insertedNode of (CreateDictNode node) -> node

    let leftHeight = getMaybeHeight insertedRecord.leftLeaf -- height of left leaf, next heights of leaves of this leaf
    let
      leftLeftHeight = case insertedRecord.leftLeaf of
        Just (CreateDictNode node1) -> getMaybeHeight node1.leftLeaf
        _ -> getMaybeHeight Nothing
    let
      rightLeftHeight = case insertedRecord.leftLeaf of
        Just (CreateDictNode node1) -> getMaybeHeight node1.rightLeaf
        _ -> getMaybeHeight Nothing
    let rightHeight = getMaybeHeight insertedRecord.rightLeaf -- height of right leaf, next heights of leaves of this leaf
    let
      leftRightHeight = case insertedRecord.rightLeaf of
        Just (CreateDictNode node1) -> getMaybeHeight node1.leftLeaf
        _ -> getMaybeHeight Nothing
    let
      rightRightHeight = case insertedRecord.rightLeaf of
        Just (CreateDictNode node1) -> getMaybeHeight node1.rightLeaf
        _ -> getMaybeHeight Nothing
    let
      balancedNode
        | leftHeight - rightHeight > 1 && rightLeftHeight > leftLeftHeight = rightTurnInternal $
            case insertedRecord.leftLeaf of
              Just node -> singletonNode insertedRecord.key insertedRecord.value (Just $ leftTurnInternal node) insertedRecord.rightLeaf insertedRecord.height
              Nothing -> (CreateDictNode insertedRecord)
        | leftHeight - rightHeight > 1 = rightTurnInternal insertedNode
        | leftHeight - rightHeight < -1 && rightRightHeight > leftRightHeight = leftTurnInternal $
            case insertedRecord.rightLeaf of
              Just node -> singletonNode insertedRecord.key insertedRecord.value insertedRecord.leftLeaf (Just $ rightTurnInternal node) insertedRecord.height
              Nothing -> (CreateDictNode insertedRecord)
        | leftHeight - rightHeight < -1 = leftTurnInternal insertedNode
        | otherwise = insertedNode

    balancedNode

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

leftTurnInternal :: forall a b. DictNode a b -> DictNode a b
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

rightTurnInternal :: forall a b. DictNode a b -> DictNode a b
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
  getInternal (Just (CreateDictNode node))
    | key == node.key = Just node.value
    | key > node.key = getInternal node.rightLeaf
    | key < node.key = getInternal node.leftLeaf
    | otherwise = Nothing

balanceTree :: forall a b. Dict a b -> Dict a b
balanceTree (CreateDict dict) = CreateDict ({ root: balanceTreeInternal dict.root })
  where
  balanceTreeInternal :: Maybe (DictNode a b) -> Maybe (DictNode a b)
  balanceTreeInternal Nothing = Nothing
  balanceTreeInternal (Just (CreateDictNode node)) = do
    let left = balanceTreeInternal node.leftLeaf
    let right = balanceTreeInternal node.rightLeaf
    let newLeftHeight = getMaybeHeight $ left
    let newRightHeight = getMaybeHeight $ right
    let
      newTree
        | newLeftHeight - newRightHeight > 1 = rightTurn $ CreateDict ({ root: Just $ singletonNode node.key node.value left right node.height })
        | newLeftHeight - newRightHeight < -1 = leftTurn $ CreateDict ({ root: Just $ singletonNode node.key node.value left right node.height })
        | otherwise = CreateDict ({ root: Just $ (CreateDictNode node) })
    let newNode (CreateDict tempDict) = tempDict.root
    newNode newTree