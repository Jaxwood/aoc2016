module Day1 (parseInput) where

  import Data.List.Split

  data Pole = North | South | West | East | Center deriving Show

  data Point = Point Integer Integer deriving Show

  data Direction = Direction Point Pole deriving Show

  parseInput :: String -> [String]
  parseInput directions =
    splitOn "," directions
    