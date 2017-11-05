module Day1 (parseInput, toMove, Move(R, L)) where

  import Data.List.Split
  import qualified Data.Text as T

  data Move = R Int | L Int deriving (Eq, Show)

  parseInput :: String -> [String]
  parseInput directions =
    map trim $ splitOn "," directions

  toMove :: String -> Move
  toMove move@(x:xs) 
    | x == 'R' = R (read xs)
    | otherwise = L (read xs)
    
  trim :: (String -> String)
  trim =
    T.unpack . T.strip . T.pack
    