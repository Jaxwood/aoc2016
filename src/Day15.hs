module Day15 (resulta) where

  import Data.Monoid

  -- check if the disc is in the open position
  valid :: (Int, Int) -> Int -> All
  valid (pos, idx) time = All $ (idx + time) `mod` pos == 0

  -- move the discs
  generate :: [(Int, Int)] -> Int -> All
  generate discs num = mconcat $ zipWith valid discs [(num + 1)..]

  -- keep moving the disc until everything is aligned
  resulta :: [(Int, Int)] -> Int
  resulta discs = length $ takeWhile (not . getAll) $ map (generate discs) [0..]
