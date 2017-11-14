module Day3 (resulta, resultb) where

  import Data.List.Split
  import Data.List

  resulta :: String -> Int
  resulta =  filterByValid . rows

  resultb :: String -> Int
  resultb = filterByValid . columns

  filterByValid :: [[Int]] -> Int
  filterByValid = length . filter (isValid . toMax)

  isValid :: (Int, Int) -> Bool
  isValid a = fst a > snd a

  toMax :: [Int] -> (Int, Int)
  toMax xs =
    let m = maximum xs in
    ((sum xs) - m, m)

  columns :: String -> [[Int]]
  columns = chunksOf 3 . concat . transpose . rows

  rows :: String -> [[Int]]
  rows = map (map read . words) . lines
