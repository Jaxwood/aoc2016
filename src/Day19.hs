module Day19 (resulta) where

  data Inventory = Present Int deriving (Eq,Show)

  game :: [Inventory] -> [Inventory] -> Int
  game ((Present x):[]) [] = x
  game acc [] = game [] (reverse acc)
  game acc ((Present x):[]) = game (acc ++ [(Present x)]) []
  game acc ((Present x):(Present y):ts) = game ((Present x):acc) ts

  resulta :: Int -> Int
  resulta num = game [] participants
    where participants = map Present [1..num]
