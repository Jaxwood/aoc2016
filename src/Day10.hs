module Day10 (resulta, resultb) where

  import Data.List

  resulta :: String -> [Int] -> Int
  resulta x xs = head xs

  resultb :: String -> String
  resultb = id
