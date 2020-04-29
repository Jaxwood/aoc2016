module Day15 (resulta) where

  import Data.Monoid

  valid :: (Int, Int) -> Int -> All
  valid (pos, idx) time = All $ (idx + time) `mod` pos == 0

  generate :: [(Int, Int)] -> Int -> All
  generate discs num = mconcat $ zipWith valid discs [(num + 1)..]

  resulta :: [(Int, Int)] -> Int
  resulta discs = length lst
    where lst = takeWhile (not . getAll) $ map (generate discs) [0..]
