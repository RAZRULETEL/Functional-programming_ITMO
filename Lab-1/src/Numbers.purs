module Numbers where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.String (split)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Data.String.Pattern (Pattern(..))
import Data.Function (($))
import Data.Array (foldl, head, length, tail)
import Data.Maybe (fromMaybe)

import Type.Proxy (Proxy(Proxy))
import Data.Semiring (add)
import Data.BigInt (BigInt, fromString, fromTLInt, toString)
import Data.Boolean (otherwise)

bigZero = fromTLInt (Proxy :: Proxy 0)

sumRecursive :: Array BigInt -> BigInt
sumRecursive [] = bigZero
sumRecursive arr = add (fromMaybe bigZero $ head arr) (sumRecursive $ fromMaybe [] $ tail arr)

sumTailRecursive :: Array BigInt -> BigInt
sumTailRecursive arr = sumTailRecursiveInternal arr bigZero

sumTailRecursiveInternal :: Array BigInt -> BigInt -> BigInt
sumTailRecursiveInternal [] sum = sum
sumTailRecursiveInternal arr sum = sumTailRecursiveInternal (fromMaybe [] $ tail arr) (sum `add` (fromMaybe bigZero $ head arr))

sumCase :: Array BigInt -> BigInt
sumCase arr = case arr of
  [] -> bigZero
  _ -> add (fromMaybe bigZero $ head arr) (sumCase $ fromMaybe [] $ tail arr)

sumGuard :: Array BigInt -> BigInt
sumGuard arr
  | (length arr) == 0 = bigZero
  | otherwise = add (fromMaybe bigZero $ head arr) (sumCase $ fromMaybe [] $ tail arr)

sumFold :: String -> String
sumFold text = toString $ foldl add bigZero (map strToBigInt $ splitByLine text)
  where
  splitByLine :: String -> Array String
  splitByLine text = split (Pattern "\n") text

  strToBigInt :: String -> BigInt
  strToBigInt text = fromMaybe bigZero (fromString text)

textToIntArray :: String -> Array BigInt
textToIntArray text = map (\n -> fromMaybe bigZero (fromString n)) (split (Pattern "\n") text)

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "src/numbers.txt"

  let bigArr = textToIntArray text

  log $ "Usual recursion: " <> (toString $ sumRecursive bigArr)
  log $ "Tail recursion:  " <> (toString $ sumTailRecursive bigArr)
  log $ "Case recursion:  " <> (toString $ sumCase bigArr)
  log $ "Guard recursion: " <> (toString $ sumGuard bigArr)
  log $ "Fold implementation: " <> sumFold text
