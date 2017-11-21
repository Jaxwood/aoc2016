module Day6 (resulta, resultb) where

  import Data.List
  import Data.Function

  resulta :: String -> String
  resulta = map common . tuple . map (group . sort) . transpose . lines

  resultb :: String -> String
  resultb = id

  common :: [(String, Int)] -> Char
  common s = head . fst . last $ sortBy (compare `on` snd) s

  tuple :: [[String]] -> [[(String, Int)]]
  tuple = map (\x -> map (\y -> (y, length y)) x)
