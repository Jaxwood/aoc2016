module Day6 (resulta, resultb) where

  import Data.List
  import Data.Function

  resulta :: String -> String
  resulta = map mostCommon . tuple . map (group . sort) . transpose . lines

  resultb :: String -> String
  resultb = map leastCommon . tuple . map (group . sort) . transpose . lines

  leastCommon :: [(String, Int)] -> Char
  leastCommon s = head . fst . head $ sortBy (compare `on` snd) s

  mostCommon :: [(String, Int)] -> Char
  mostCommon s = head . fst . last $ sortBy (compare `on` snd) s

  tuple :: [[String]] -> [[(String, Int)]]
  tuple = map (\x -> map (\y -> (y, length y)) x)
