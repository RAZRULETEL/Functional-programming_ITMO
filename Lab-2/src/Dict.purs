module Dict where

import Prelude

import Data.Maybe (Maybe, Maybe(Nothing), Maybe(..))
import Data.Boolean (otherwise)
import Data.Show (show)
import Data.Tuple (Tuple(Tuple), fst, snd, uncurry)

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

instance (Show a, Show b) => Show (Dict a b) where
  show (CreateDict a) = show a

instance (Show a, Show b) => Show (DictNode a b) where
  show (CreateDictNode node) = "{ key: " <> show node.key <> ", value: " <> show node.value <> ", height: " <> show node.height <> ", left: " <> show node.leftLeaf <> ", right: " <> show node.rightLeaf <> " }"

instance (Eq a, Eq b, Ord a) => Eq (Dict a b) where
  eq dict1 dict2 = do
    if not (length dict1 == length dict2) then false
    else foldrDict (\k v acc -> acc && get dict2 k == Just v) true dict1

instance Ord a => Semigroup (Dict a b) where
  append (CreateDict dict1) (CreateDict dict2) = appendInternal (CreateDict dict1) dict2.root
    where
    appendInternal :: Dict a b -> Maybe (DictNode a b) -> Dict a b
    appendInternal dictSum Nothing = dictSum
    appendInternal dictSum (Just (CreateDictNode node)) = do
      let summed = appendInternal (appendInternal dictSum node.leftLeaf) node.rightLeaf
      insert summed node.key node.value

instance Ord a => Monoid (Dict a b) where
  mempty = (CreateDict { root: Nothing })

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
              Just node -> singletonNode
                insertedRecord.key
                insertedRecord.value
                (Just $ leftTurnInternal node)
                insertedRecord.rightLeaf
                (1 + max (getHeight $ leftTurnInternal node) (getMaybeHeight insertedRecord.rightLeaf))
              Nothing -> (CreateDictNode insertedRecord)
        | leftHeight - rightHeight > 1 = rightTurnInternal insertedNode
        | leftHeight - rightHeight < -1 && leftRightHeight > rightRightHeight = leftTurnInternal $
            case insertedRecord.rightLeaf of
              Just node -> singletonNode
                insertedRecord.key
                insertedRecord.value
                insertedRecord.leftLeaf
                (Just $ rightTurnInternal node)
                (1 + max (getHeight $ rightTurnInternal node) (getMaybeHeight insertedRecord.leftLeaf))
              Nothing -> (CreateDictNode insertedRecord)
        | leftHeight - rightHeight < -1 = leftTurnInternal insertedNode
        | otherwise = insertedNode

    balancedNode

remove :: forall a b. Ord a => Dict a b -> a -> Dict a b
remove (CreateDict dict) key = CreateDict ({ root: removeInternal dict.root })
  where
  removeInternal :: Ord a => Maybe (DictNode a b) -> Maybe (DictNode a b)
  removeInternal Nothing = Nothing -- nothing to do with empty dict
  removeInternal (Just (CreateDictNode node)) = do
    let
      removeSubTree
        | key > node.key =
            do
              let removedNode = removeInternal node.rightLeaf
              Just
                $ singletonNode
                    node.key
                    node.value
                    node.leftLeaf
                    removedNode
                $ 1 + max (getMaybeHeight node.leftLeaf) (getMaybeHeight removedNode)
        | key < node.key =
            do
              let removedNode = removeInternal node.leftLeaf
              Just
                $ singletonNode
                    node.key
                    node.value
                    removedNode
                    node.rightLeaf
                $ 1 + max (getMaybeHeight node.rightLeaf) (getMaybeHeight removedNode)
        | otherwise = do
            let leftHeight = getMaybeHeight node.leftLeaf -- height of left leaf, next heights of leaves of this leaf
            let rightHeight = getMaybeHeight node.rightLeaf -- height of right leaf, next heights of leaves of this leaf

            let biggestSubTree = if (rightHeight > leftHeight) then node.rightLeaf else node.leftLeaf
            case biggestSubTree of -- target node find and have left leaf
              Nothing -> node.rightLeaf
              Just (CreateDictNode leftNode) -> do
                let
                  getHighest (CreateDictNode parent) = case if (rightHeight > leftHeight) then parent.leftLeaf else parent.rightLeaf of
                    Nothing -> Tuple (if (rightHeight > leftHeight) then parent.rightLeaf else parent.leftLeaf) parent
                    Just rightChild ->
                      uncurry
                        ( \subTree highest ->
                            ( Tuple
                                ( Just
                                    $ singletonNode
                                        parent.key
                                        parent.value
                                        (if (rightHeight > leftHeight) then subTree else parent.leftLeaf)
                                        (if (rightHeight > leftHeight) then parent.rightLeaf else subTree)
                                    $ 1 + max (getMaybeHeight subTree) (getMaybeHeight $ if (rightHeight > leftHeight) then parent.rightLeaf else parent.leftLeaf)
                                )
                                highest
                            )
                        ) $ getHighest rightChild

                uncurry
                  ( \subTree highest -> Just
                      $ singletonNode
                          highest.key
                          highest.value
                          (if (rightHeight > leftHeight) then node.leftLeaf else subTree)
                          (if (rightHeight > leftHeight) then subTree else node.rightLeaf)
                      $ 1 + max (getMaybeHeight subTree) (getMaybeHeight leftNode.leftLeaf)
                  ) $ getHighest (CreateDictNode leftNode)

    case removeSubTree of
      Nothing -> removeSubTree
      Just (CreateDictNode insertedRecord) -> do
        let leftHeight = getMaybeHeight node.leftLeaf -- height of left leaf, next heights of leaves of this leaf
        let
          leftLeftHeight = case insertedRecord.leftLeaf of
            Just (CreateDictNode node1) -> getMaybeHeight node1.leftLeaf
            _ -> getMaybeHeight Nothing
        let
          rightLeftHeight = case insertedRecord.leftLeaf of
            Just (CreateDictNode node1) -> getMaybeHeight node1.rightLeaf
            _ -> getMaybeHeight Nothing
        let rightHeight = getMaybeHeight node.rightLeaf -- height of right leaf, next heights of leaves of this leaf
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
            | leftHeight - rightHeight > 1 = rightTurnInternal (CreateDictNode insertedRecord)
            | leftHeight - rightHeight < -1 && rightRightHeight > leftRightHeight = leftTurnInternal $
                case insertedRecord.rightLeaf of
                  Just node -> singletonNode insertedRecord.key insertedRecord.value insertedRecord.leftLeaf (Just $ rightTurnInternal node) insertedRecord.height
                  Nothing -> (CreateDictNode insertedRecord)
            | leftHeight - rightHeight < -1 = leftTurnInternal (CreateDictNode insertedRecord)
            | otherwise = (CreateDictNode insertedRecord)

        Just balancedNode

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

filter :: forall a b. Ord a => (a -> b -> Boolean) -> Dict a b -> Dict a b
filter func (CreateDict dict) = filterInternal (CreateDict dict) dict.root
  where
  filterInternal :: Dict a b -> Maybe (DictNode a b) -> Dict a b
  filterInternal dict Nothing = dict
  filterInternal dict (Just (CreateDictNode node)) = do
    let filtered = filterInternal (filterInternal dict node.leftLeaf) node.rightLeaf
    if (func node.key node.value) then (remove filtered node.key) else filtered

map :: forall a b c d. Ord a => Ord c => (a -> b -> Tuple c d) -> Dict a b -> Dict c d
map func (CreateDict dict) = mapInternal (CreateDict { root: Nothing }) dict.root
  where
  mapInternal :: Dict c d -> Maybe (DictNode a b) -> Dict c d
  mapInternal dict Nothing = dict
  mapInternal dict (Just (CreateDictNode node)) = do
    let mapped = mapInternal (mapInternal dict node.leftLeaf) node.rightLeaf
    let mapResult = func node.key node.value
    insert mapped (fst mapResult) (snd mapResult)

foldrDict :: forall a b c. (a -> b -> c -> c) -> c -> Dict a b -> c
foldrDict func init (CreateDict dict) = case dict.root of
  Nothing -> init
  Just root -> foldrDictInternal root init
    where
    foldrDictInternal :: DictNode a b -> c -> c
    foldrDictInternal (CreateDictNode node) c = do
      let
        rightSubTreeResult = case node.rightLeaf of
          Just right -> foldrDictInternal right c
          Nothing -> c
      let selfComputed = func node.key node.value rightSubTreeResult
      case node.leftLeaf of
        Just left -> foldrDictInternal left selfComputed
        Nothing -> selfComputed

foldlDict :: forall a b c. (a -> c -> b -> c) -> c -> Dict a b -> c
foldlDict func init (CreateDict dict) = case dict.root of
  Nothing -> init
  Just root -> foldlDictInternal root init
    where
    foldlDictInternal :: DictNode a b -> c -> c
    foldlDictInternal (CreateDictNode node) c = do
      let
        leftSubTreeResult = case node.leftLeaf of
          Just right -> foldlDictInternal right c
          Nothing -> c
      let selfComputed = func node.key leftSubTreeResult node.value
      case node.rightLeaf of
        Just left -> foldlDictInternal left selfComputed
        Nothing -> selfComputed
