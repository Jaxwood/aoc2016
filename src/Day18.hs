module Day18 (resulta) where

  import Control.Monad

  data Tile = Safe | Trap deriving (Eq,Show)

  -- parse the string representation into a Tile
  toTile :: Char -> Tile
  toTile '^' = Trap
  toTile '.' = Safe

  -- generate the next row
  row :: [Tile] -> [Tile] -> [Tile]
  row acc (a:b:[]) = (reverse acc)
  row acc ((Trap):(Trap):(Safe):ts) = row (Trap:acc) ((Trap):(Safe):ts) 
  row acc ((Safe):(Trap):(Trap):ts) = row (Trap:acc) ((Trap):(Trap):ts) 
  row acc ((Trap):(Safe):(Safe):ts) = row (Trap:acc) ((Safe):(Safe):ts)
  row acc ((Safe):(Safe):(Trap):ts) = row (Trap:acc) ((Safe):(Trap):ts)
  row acc ts = row (Safe:acc) (tail ts)

  -- run algorithm number of times
  run :: Int -> [[Tile]] -> [Tile] -> Int
  run 0 acc _ = length $ filter (==Safe) $ join acc
  run num acc ts = run (num - 1) (ts':acc) next
    where ts' = row [] ts
          next = Safe:ts'++[Safe]

  resulta :: Int -> String -> Int
  resulta num a = run (num - 1) [ts] (Safe:ts ++ [Safe])
    where ts = map toTile a
