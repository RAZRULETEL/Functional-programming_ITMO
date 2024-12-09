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
import Data.List (length, slice, snoc, toUnfoldable)
import Data.Array as Array
import Data.List.Types (List)
import Data.Number.Format (fixed, toStringWith)
import Data.Array (foldl)

defaultStep :: Number
defaultStep = 1.0

getArgNumberOrDefault :: Int -> Array String -> Number -> Number
getArgNumberOrDefault i args default = fromMaybe default $ fromString $ fromMaybe "" (Array.index args i)

printPointsArray :: List (Tuple Number Number) -> String
printPointsArray points =
  ("\n" <> (joinWith "\t" $ toUnfoldable $ map (toStringWith (fixed 2)) $ map fst points)) <>
    ( "\n" <> (joinWith "\t" $ toUnfoldable $ map (toStringWith (fixed 2)) $ map snd points)
    )

strToPoint :: String -> Maybe (Tuple Number Number)
strToPoint str =
  do
    let arr = map (\el -> fromString el) $ split (Pattern " ") $ trim str
    let x = fromMaybe Nothing (Array.index arr 0)
    let y = fromMaybe Nothing (Array.index arr 1)

    case Tuple x y of
      (Tuple (Just xv) (Just yv)) -> Just $ Tuple xv yv
      _ -> Nothing

interpolateWithMethods :: List (Tuple Number Number) -> Array (Tuple String Interpolator) -> Number -> Tuple String (List (Tuple Number Number))
interpolateWithMethods points methods freq =
  do
    let
      results = map
        ( \(Tuple name method) ->
            do
              let result = method points freq
              let resPoints = snd result

              Tuple
                ( if length resPoints > 0 then ("\n" <> name <> " interpolation:" <> (printPointsArray resPoints))
                  else ""
                )
                $ fst result
        )
        methods

    let outStr = joinWith "" $ map fst results
    let maxPoints = foldl (\max e -> if e > max then e else max) 0 $ map snd results

    Tuple outStr $ slice (length points - maxPoints) (length points) points

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

      case strToPoint s of
        Nothing -> log $ "Wrong input, you must enter two numbers splitted by space"
        (Just tuple) -> do
          let res = interpolateWithMethods (snoc old tuple) useMethods frequency
          Ref.write (snd res) points
          log $ fst res
      prompt interface
  prompt interface
