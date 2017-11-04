module Day1 where
  data Pole = North | South | West | East deriving Show

  data Point = Point Integer Integer deriving Show

  data Direction = Direction Point Pole deriving Show

  getShortestPath directions = 0