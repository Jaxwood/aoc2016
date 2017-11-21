module Day7 (resulta, resultb, palindrome) where

  import Data.List
  import Data.Function

  data IPv7 = IPv7

  resulta :: String -> String
  resulta = id

  resultb :: String -> String
  resultb = id

  palindrome :: String -> Bool
  palindrome s@(a:b:c:d:xs)
    | a == d && b == c && a /= b && c /= d = True
    | otherwise = palindrome $ tail s
  palindrome _ =
    False

  parse :: String -> IPv7
  parse s = IPv7
