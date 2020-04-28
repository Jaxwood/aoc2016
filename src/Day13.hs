module Day13 (resulta) where

  import Data.Bits

  bitcount :: Int -> Int -> Int
  bitcount num acc
    | num == 0 = acc
    | otherwise = bitcount (num .&. (num - 1)) (acc + 1)
  
  open :: Int -> (Int, Int) -> Bool
  open num (x, y) = let candidate = (x * x) + (3 * x) + (2*x*y) + y + (y*y) + num
                    in even $ bitcount candidate 0

  resulta :: (Int, Int)  -> Int -> Bool
  resulta (x,y) a = open a (x,y)
