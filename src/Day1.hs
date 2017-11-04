module Day1 (getShortestPath) where
  data Pole = North | South | West | East deriving Show

  data Point = Point Integer Integer deriving Show

  data Direction = Direction Point Pole deriving Show

  getShortestPath :: String -> Int
  getShortestPath directions = 0