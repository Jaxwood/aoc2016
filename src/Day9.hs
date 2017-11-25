module Day9 (resulta, resultb) where

  import Data.Char
  import Data.Either (rights)
  import Data.List

  resulta :: String -> String
  resulta = match

  resultb :: String -> Int
  resultb = length

  match :: String -> String
  match [] =
    ""

  match ('(':a:'x':b:')':xs) =
    let r = take a' xs
    in (concat $ take b' $ repeat r) ++ (match (drop a' xs))
    where a' = digitToInt a
          b' = digitToInt b

  match (x:xs) =
    x:(match xs)
