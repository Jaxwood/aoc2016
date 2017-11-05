module Day1 (parseInput, toMove, moveList, Move(R, L)) where

  import Data.List.Split
  import qualified Data.Text as T

  data Move = R Int | L Int deriving (Eq, Show)

  parseInput :: String -> [String]
  parseInput directions =
    map trim $ splitOn "," directions
  
  moveList :: String -> [Move]
  moveList = map toMove . parseInput

  toMove :: String -> Move
  toMove move@(x:xs) 
    | x == 'R' = R (read xs)
    | otherwise = L (read xs)
    
  trim :: String -> String
  trim = T.unpack . T.strip . T.pack 