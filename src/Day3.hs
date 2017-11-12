module Day3 (resulta, resultb) where

  import Data.List.Split
  import Data.List

  resulta :: String -> Int
  resulta = length . filter (isValid . toMax) . rows

  resultb :: String -> Int
  resultb = length . filter (isValid . toMax) . columns

  isValid :: (Int, Int) -> Bool
  isValid a = fst a > snd a

  toMax :: [Int] -> (Int, Int)
  toMax xs =
    let m = maximum xs in
    ((sum xs) - m, m)

  columns :: String -> [[Int]]
  columns = chunksOf 3 . concat . transpose . map (map read . words) . lines

  rows :: String -> [[Int]]
  rows = map (map read . words) . lines
