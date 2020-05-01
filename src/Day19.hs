module Day19 (resulta, resultb) where

  import qualified Data.Sequence as S
  import Data.Maybe

  game :: [Maybe Int] -> [Maybe Int] -> Int
  game ((Just x):[]) [] = x
  game acc [] = game [] (reverse acc)
  game acc ((Just x):[]) = game (acc ++ [(Just x)]) []
  game acc ((Just x):(Just y):ts) = game ((Just x):acc) ts

  resulta :: Int -> Int
  resulta num = game [] participants
    where participants = map Just [1..num]

  game' :: Int -> S.Seq (Maybe Int) -> S.Seq (Maybe Int)
  game' idx participants = if S.length participants == 1 then participants else game' idx' next
    where len = S.length participants
          newIdx = ((len `div` 2) + idx) `mod` len
          (a,b) = S.splitAt newIdx participants
          next = (S.><) a (S.drop 1 b)
          loc = if newIdx >= idx then idx else pred idx
          idx' = if loc == S.length next - 1 then 0 else succ loc


  resultb :: Int -> Int -> S.Seq (Maybe Int)
  resultb idx num = game' idx participants
    where participants = S.fromList $ map Just [1..num]
