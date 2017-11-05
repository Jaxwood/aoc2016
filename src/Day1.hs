module Day1 (parseInput, move, moves, Move(R, L)) where

  import Data.List.Split
  import qualified Data.Text as T

  data Move = R Int | L Int deriving (Eq, Show)

  parseInput :: String -> [String]
  parseInput = map trim . splitOn ","
  
  moves :: String -> [Move]
  moves = map move . parseInput

  move :: String -> Move
  move m@(x:xs) 
    | x == 'R' = R (read xs)
    | otherwise = L (read xs)
    
  trim :: String -> String
  trim = T.unpack . T.strip . T.pack 
