module Main where

import Prelude

import Effect (Effect, foreachE)
import Effect.Console (log)
import Node.Process (argv)
import Data.Number (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Ref (new, read, write) as Ref
import Data.String.Pattern (Pattern(..))
import Node.ReadLine (close, createConsoleInterface, lineH, noCompletion, prompt, setPrompt)
import Node.EventEmitter (on_)
import Data.String.Common (joinWith, split, trim)
import Data.Tuple (Tuple(..), fst, snd)
import Interpolation (Interpolator, calcSplitDifference, linearInterpolate, newtonInterpolate)
import Data.Show (show)
import Data.Monoid (mempty) as List
import Data.List (length, snoc, toUnfoldable)
import Data.Array as Array
import Data.List.Types (List)
import Data.Number.Format (fixed, toStringWith)

defaultStep :: Number
defaultStep = 1.0

getArgNumberOrDefault :: Int -> Array String -> Number -> Number
getArgNumberOrDefault i args default = fromMaybe default $ fromString $ fromMaybe "" (Array.index args i)

printPointsArray :: List (Tuple Number Number) -> Effect Unit
printPointsArray points =
  do
    log $ joinWith "\t" $ toUnfoldable $ map (toStringWith (fixed 2)) $ map fst points
    log $ joinWith "\t" $ toUnfoldable $ map (toStringWith (fixed 2)) $ map snd points

main :: Effect Unit
main = do

  args <- argv

  let frequency = getArgNumberOrDefault 2 args defaultStep
  let methods = fromMaybe "linear,newton" (Array.index args 3)

  let
    useMethods :: Array (Tuple String Interpolator)
    useMethods = Array.filter (\(Tuple name _) -> not $ name == "none")
      $ map
          ( \method -> case method of
              "linear" -> Tuple "Linear" linearInterpolate
              "newton" -> Tuple "Newton" newtonInterpolate
              _ -> Tuple "none" linearInterpolate
          )
      $ split (Pattern ",") methods

  log $ "Sampling frequency: " <> (show frequency)

  points <- Ref.new List.mempty
  interface <- createConsoleInterface noCompletion
  setPrompt "> " interface
  interface # on_ lineH \s ->
    if s == "quit" then close interface
    else do
      old <- Ref.read points

      let arr = map (\el -> fromString el) $ split (Pattern " ") $ trim s
      let x = fromMaybe Nothing (Array.index arr 0)
      let y = fromMaybe Nothing (Array.index arr 1)

      let
        point = case Tuple x y of
          (Tuple (Just xv) (Just yv)) -> Just $ Tuple xv yv
          _ -> Nothing

      case point of
        Nothing -> log $ "Wrong input, you must enter two numbers splitted by space"
        (Just tuple) -> do
          let newPoints = (snoc old tuple)

          Ref.write newPoints points
          log $ "You typed: " <> show tuple <> ", total points: " <> (show $ length newPoints)

          foreachE useMethods
            ( \(Tuple name method) ->
                do
                  let result = method newPoints frequency
                  when (length result > 0)
                    do
                      log $ name <> " interpolation:"
                      printPointsArray result
            )
      prompt interface
  prompt interface
