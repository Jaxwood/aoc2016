module Day8 (resulta, resultb, rotate) where

  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  rect :: [[Int]]
  rect = concat [take 6 $ repeat x | x <- [take 50 $ repeat 0]]

  resulta :: String -> Int
  resulta s = 0

  resultb :: String -> Int
  resultb s = 0

  rotate :: [Int] -> Int -> [Int]
  rotate xs i
    | i > ln = rotate' xs $ ln - m
    | otherwise = rotate' xs $ ln - i
    where ln = length xs
          m = i `mod` ln

  rotate' :: [Int] -> Int -> [Int]
  rotate' xs i =
    let (a, b) = splitAt i xs
    in b ++ a

