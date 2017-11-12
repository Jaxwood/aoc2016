module Day2 (resulta) where

  data Move = U (Maybe Int) | D (Maybe Int) | L (Maybe Int) | R (Maybe Int)

  resulta :: String -> Int
  resulta = length . lines
