module Interpolation where

import Prelude
import Data.List.Types (List)
import Data.Tuple (Tuple(..), fst, snd)
import Data.List (deleteAt, foldl, index, length, range, slice, unsnoc)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty) as List
import Generator (generate)
import Data.FunctorWithIndex (mapWithIndex)

type Interpolator = List (Tuple Number Number) -> Number -> Tuple Int (List (Tuple Number Number))

newtonCountOfPoints :: Int
newtonCountOfPoints = 5

linearInterpolate :: List (Tuple Number Number) -> Number -> Tuple Int (List (Tuple Number Number))
linearInterpolate inputPoints step =
  case unsnoc inputPoints of
    Nothing -> Tuple (length inputPoints) List.mempty
    (Just { init: list, last: p2 }) ->
      case unsnoc list of
        Nothing -> Tuple (length inputPoints) List.mempty
        (Just { last: p1 }) ->
          Tuple 2
            $ map
                ( \x -> Tuple x $
                    (snd p1 * (fst p2 - x) + snd p2 * (x - fst p1))
                      / (fst p2 - fst p1)
                )
            $ generate step (fst p1) (fst p2)

foldSum :: List Number -> Number
foldSum = foldl (\acc el -> acc + el) 0.0

foldMul :: List Number -> Number
foldMul = foldl (\acc el -> acc * el) 1.0

calcSplitDifference :: List (Tuple Number Number) -> Number
calcSplitDifference list =
  foldSum
    $ mapWithIndex
        ( \i ei ->
            let
              remainingList = fromMaybe List.mempty (deleteAt i list)
            in
              case length remainingList of
                0 -> snd ei
                _ -> snd ei / foldMul (map (\(Tuple xj _) -> fst ei - xj) remainingList)
        )
        list

newtonInterpolate :: List (Tuple Number Number) -> Number -> Tuple Int (List (Tuple Number Number))
newtonInterpolate inputPoints step =
  do
    if length inputPoints < newtonCountOfPoints then Tuple (length inputPoints) List.mempty
    else
      do
        let
          calcInterpolation :: Number -> Tuple Number Number
          calcInterpolation x = Tuple x
            $ foldSum
            $ map
                ( \i -> calcSplitDifference (slice 0 (i + 1) inputPoints)
                    *
                      if i == 0 then 1.0
                      else
                        ( foldMul
                            $ map
                                (\point -> x - fst point)
                            $ slice 0 i inputPoints
                        )
                )
            $ range 0
            $ length inputPoints - 1

        let mfp = index inputPoints (length inputPoints - newtonCountOfPoints)
        let mlp = index inputPoints (length inputPoints - 1)

        case Tuple mfp mlp of
          Tuple (Just fp) (Just lp) ->
            Tuple newtonCountOfPoints
              $ map
                  calcInterpolation
              $ generate step (fst fp) (fst lp)
          _ -> Tuple (length inputPoints) List.mempty
