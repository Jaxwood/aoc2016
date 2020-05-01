module Day20 (resulta) where

  import Data.List.Split
  import qualified Data.List as L

  sorter :: [Int] -> [Int] -> Ordering
  sorter [a,_] [b,_] = compare a b

  inRange :: [Int] -> Int -> Bool
  inRange [a,b] i = i >= a && i <= b

  notBlacklisted :: [[Int]] -> [Int] -> Int
  notBlacklisted ([a,b]:rs) [l,h] 
    | inRange [l,h] (pred a) == False = succ h
    | inRange [l,h] b == False = notBlacklisted rs [l,b]
    | otherwise = notBlacklisted rs [l,h]

  resulta :: String -> Int
  resulta a = notBlacklisted (tail blacklist) (head blacklist)
    where blacklist = L.sortBy sorter $ map (map read . splitOn "-") $ lines a
