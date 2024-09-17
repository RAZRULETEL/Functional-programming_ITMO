module Triangle where

import Prelude

import Effect (Effect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Effect.Console (log)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (index, init, last, length, modifyAt, singleton, tail, (..))
import Data.Int (fromString, toStringAs)
import Data.Ord (max)
import Data.Int (decimal) as Radix

--splitByNewLine :: String -> Array String
--splitByNewLine text = (split (Pattern "\n") text)

recursiveTriangle :: String -> Int
recursiveTriangle text = liftUpRows $ triangulate text
    where
    triangulate :: String -> Array (Array Int)
    triangulate text = map (\line -> map (\e -> fromMaybe 0 $ fromString e) (split (Pattern " ") line)) (split (Pattern "\n") text)

    liftUpRows :: Array (Array Int) -> Int
    liftUpRows arr = case length arr of
                        1 -> fromMaybe 0 $ index (fromMaybe (singleton 0) $ index arr 0) 0
                        _ -> liftUpRows $ fromMaybe [] $ init $ liftLastRow arr
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

    let recursiveResult = recursiveTriangle text


    log text
    log $ toStringAs (Radix.decimal) recursiveResult

