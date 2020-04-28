module Day13 (resulta, resultb) where

  import Data.Bits
  import qualified Data.Set as S

  -- Brian Kernighan’s Algorithm
  bitcount :: Int -> Int -> Int
  bitcount num acc
    | num == 0 = acc
    | otherwise = bitcount (num .&. (num - 1)) (acc + 1)

  -- check if the square is open
  open :: Int -> (Int, Int, Int) -> Bool
  open num (x, y, _) = let candidate = (x * x) + (3 * x) + (2*x*y) + y + (y*y) + num
                    in even $ bitcount candidate 0

  -- add two coordinates
  add :: (Int, Int, Int) -> (Int, Int) -> (Int,Int, Int)
  add (x,y,m) (x',y') = (x+x', y+y', (m+1))

  -- find neighbor squares
  neighbors :: (Int, Int, Int) -> [(Int, Int, Int)]
  neighbors coord = filter (\(x,y,_) -> and [(x >= 0), (y >= 0)]) coords
    where coords = map (add coord) [(1,0), ((-1), 0), (0, 1), (0, (-1))]

  -- explore the maze and find the moves till target square
  explore :: (Int, Int) -> Int -> S.Set (Int,Int) -> [(Int, Int, Int)] -> Int 
  explore target fav visited [] = 0
  explore target fav visited ((x,y,m):xs)
    | (x,y) == target = m
    | S.member (x,y) visited = explore target fav visited xs
    | otherwise = explore target fav (S.insert (x,y) visited) (xs ++ next)
      where next = filter (open fav) $ neighbors (x,y,m)

  resulta :: (Int, Int) -> Int -> Int
  resulta (x,y) fav = explore (x,y) fav (S.singleton (1,1)) $ filter (open fav) $ neighbors start
    where start = (1,1,0)

  -- explore the maze and find the number of squares that can be reached within 50 moves
  explore' :: Int -> S.Set (Int,Int) -> [(Int, Int, Int)] -> Int 
  explore' fav visited [] = length visited
  explore' fav visited ((x,y,m):xs)
    | m > 50 || (S.member (x,y) visited) = explore' fav visited xs
    | otherwise = explore' fav (S.insert (x,y) visited) (xs ++ next)
      where next = filter (open fav) $ neighbors (x,y,m)

  resultb :: Int -> Int
  resultb fav = explore' fav (S.singleton (1,1)) $ filter (open fav) $ neighbors start
    where start = (1,1,0)
