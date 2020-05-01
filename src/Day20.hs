module Day20 (resulta, resultb) where

  import Data.List.Split
  import qualified Data.List as L

  -- sort ip's
  sorter :: [Int] -> [Int] -> Ordering
  sorter [a,_] [b,_] = compare a b

  -- check if the value is within the blacklist range
  inRange :: [Int] -> Int -> Bool
  inRange [a,b] i = i >= a && i <= b

  -- find the first non-blacklisted ip
  notBlacklisted :: [[Int]] -> [Int] -> Int
  notBlacklisted ([a,b]:rs) [l,h] 
    | inRange [l,h] (pred a) == False = succ h
    | inRange [l,h] b == False = notBlacklisted rs [l,b]
    | otherwise = notBlacklisted rs [l,h]

  -- sum all non-blacklisted ip's
  sumNonBlacklisted :: [[Int]] -> [Int] -> Int -> Int
  sumNonBlacklisted ([]) [l,h] acc = acc 
  sumNonBlacklisted ([a,b]:rs) [l,h] acc
    | inRange [l,h] (pred a) == False = sumNonBlacklisted rs [l,b] (acc + a - (succ h))
    | inRange [l,h] b == False = sumNonBlacklisted rs [l,b] acc
    | otherwise = sumNonBlacklisted rs [l,h] acc

  resulta :: String -> Int
  resulta a = notBlacklisted (tail blacklist) (head blacklist)
    where blacklist = L.sortBy sorter $ map (map read . splitOn "-") $ lines a

  resultb :: String -> Int
  resultb a = sumNonBlacklisted (tail blacklist) (head blacklist) 0
    where blacklist = L.sortBy sorter $ map (map read . splitOn "-") $ lines a
