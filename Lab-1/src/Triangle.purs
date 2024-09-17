module Triangle where

import Prelude

import Effect (Effect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Effect.Console (log)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (index, init, length, modifyAt, singleton)
import Data.Int (fromString, toStringAs)
import Data.Int (decimal) as Radix

recursiveTriangle :: String -> Int
recursiveTriangle text = liftUpRows $ triangulate text
  where
  triangulate :: String -> Array (Array Int)
  triangulate textPyramid = map (\line -> map (\e -> fromMaybe 0 $ fromString e) (split (Pattern " ") line)) (split (Pattern "\n") textPyramid)

  liftUpRows :: Array (Array Int) -> Int
  liftUpRows pyramidArr = case length pyramidArr of
    1 -> fromMaybe 0 $ index (fromMaybe (singleton 0) $ index pyramidArr 0) 0
    _ -> liftUpRows $ fromMaybe [] $ init $ liftLastRow pyramidArr
    where

    lift :: Array Int -> Array Int -> Int -> Array Int
    lift liftingRow prevRow i = case index liftingRow i of
      Nothing -> liftingRow
      _ -> lift (fromMaybe liftingRow $ modifyAt i (\e -> e `add` max (fromMaybe 0 $ index prevRow $ i) (fromMaybe 0 $ index prevRow $ i + 1)) liftingRow) prevRow $ i + 1

    liftLastRow :: Array (Array Int) -> Array (Array Int)
    liftLastRow arr = fromMaybe arr $ modifyAt (length arr - 2) (\row -> lift row (fromMaybe [] $ index arr $ length arr - 1) 0) arr

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "src/triangle.txt"

  log $ toStringAs (Radix.decimal) $ recursiveTriangle text

