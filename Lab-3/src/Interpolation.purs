module Interpolation where

import Prelude
import Data.List.Types (List)
import Data.Tuple (Tuple(..), fst, snd)
import Data.List (unsnoc)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty) as List
import Generator (generate)
import Effect.Console (log)

type Interpolator = List (Tuple Number Number) -> Number -> List (Tuple Number Number)

linearInterpolate :: List (Tuple Number Number) -> Number -> List (Tuple Number Number)
linearInterpolate inputPoints step =
  case unsnoc inputPoints of
    Nothing -> List.mempty
    (Just { init: list, last: p2 }) ->
      case unsnoc list of
        Nothing -> List.mempty
        (Just { last: p1 }) ->
          map
            ( \x -> Tuple x $
                (snd p1 * (fst p2 - fst p1) + snd p2 * (x - fst p1))
                  / (fst p2 - fst p1)
            )
            $ generate step (fst p1) (fst p2)

