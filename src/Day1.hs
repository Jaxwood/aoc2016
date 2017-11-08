module Day1 (resulta, resultb) where

  import Data.List.Split
  import qualified Data.Text as T

  data Move = R Int | L Int
  data Position = Position Int Int
  data Pole = North | South | East | West
  data Coord = Coord Position Pole

  resultb :: String -> Int
  resultb str = 0

  resulta :: String -> Int
  resulta = blocks . calculate . moves

  blocks :: Coord -> Int
  blocks (Coord (Position x y) _) =
    (abs x) + (abs y)
  
  calculate :: [Move] -> Coord
  calculate xs = foldl acc (Coord (Position 0 0) North) xs

  acc :: Coord -> Move -> Coord
  acc (Coord (Position 0 0) _) (L a) =
    Coord (Position (0-a) 0) West

  acc (Coord (Position 0 0) _) (R a) =
    Coord (Position a 0) East

  acc (Coord (Position x y) North) (L a) =
    Coord (Position (x-a) y) West

  acc (Coord (Position x y) North) (R a) =
    Coord (Position (x+a) y) East

  acc (Coord (Position x y) South) (L a) =
    Coord (Position (x+a) y) East

  acc (Coord (Position x y) South) (R a) =
    Coord (Position (x-a) y) West

  acc (Coord (Position x y) East) (R a) =
    Coord (Position x (y-a)) South

  acc (Coord (Position x y) East) (L a) =
    Coord (Position x (y+a)) North

  acc (Coord (Position x y) West) (R a) =
    Coord (Position x (y+a)) North

  acc (Coord (Position x y) West) (L a) =
    Coord (Position x (y-a)) South

  moves :: String -> [Move]
  moves = map move . parseInput

  move :: String -> Move
  move (x:xs) 
    | x == 'R' = R (read xs)
    | otherwise = L (read xs)

  parseInput :: String -> [String]
  parseInput = map trim . splitOn ","
  
  trim :: String -> String
  trim = T.unpack . T.strip . T.pack 
