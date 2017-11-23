module Day8 (resulta, resultb, rotate) where

  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  resulta :: String -> Int
  resulta s = 0

  resultb :: String -> Int
  resultb s = 0

  rotate :: [Int] -> Int -> [Int]
  rotate xs i
    | i > (length xs) = rotate' xs (length xs - (i `mod` (length xs)))
    | otherwise = rotate' xs (length xs - i)

  rotate' :: [Int] -> Int -> [Int]
  rotate' xs i =
    let (a, b) = splitAt i xs
    in b ++ a

