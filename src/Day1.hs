module Day1 (parseInput) where

  import Data.List.Split
  import qualified Data.Text as T

  data Pole = North | South | West | East | Center deriving Show

  data Point = Point Integer Integer deriving Show

  data Direction = Direction Point Pole deriving Show

  parseInput :: String -> [String]
  parseInput directions =
    map (T.unpack . T.strip . T.pack) $ splitOn "," directions
    