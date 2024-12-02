module Generator where

import Prelude
import Data.List (insert, singleton)
import Data.List.Types (List)

generate :: Number -> Number -> Number -> List Number
generate step start end =
  do
    let
      nextNumber prev list =
        if prev >= end then list
        else nextNumber (prev + step) $ insert (prev + step) list
    nextNumber start $ singleton start
