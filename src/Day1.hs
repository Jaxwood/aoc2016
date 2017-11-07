module Day1 (result) where

  import Data.List.Split
  import qualified Data.Text as T

  data Move = R Int | L Int deriving (Eq, Show)
  data Position = Position Int Int
  data Pole = North | South | East | West | Unknown
  data Coord = Coord Position Pole

  result :: String -> Int
  result = blocks . calculate . moves

  blocks :: Coord -> Int
  blocks (Coord (Position x y) _) =
    (abs x) + (abs y)
  
  calculate :: [Move] -> Coord
  calculate xs = foldl acc (Coord (Position 0 0) Unknown) xs

  acc :: Coord -> Move -> Coord
  acc (Coord (Position 0 0) _) move@(L a) =
    Coord (Position (0-a) 0) West

  acc (Coord (Position 0 0) _) move@(R a) =
    Coord (Position a 0) East

  acc (Coord (Position x y) North) move@(L a) =
    Coord (Position (x-a) y) West

  acc (Coord (Position x y) North) move@(R a) =
    Coord (Position (x+a) y) East

  acc (Coord (Position x y) South) move@(L a) =
    Coord (Position (x+a) y) East

  acc (Coord (Position x y) South) move@(R a) =
    Coord (Position (x-a) y) West

  acc (Coord (Position x y) East) move@(R a) =
    Coord (Position x (y-a)) South

  acc (Coord (Position x y) East) move@(L a) =
    Coord (Position x (y+a)) North

  acc (Coord (Position x y) West) move@(R a) =
    Coord (Position x (y+a)) North

  acc (Coord (Position x y) West) move@(L a) =
    Coord (Position x (y-a)) South

  acc (Coord (Position x y) _) move =
    Coord (Position 0 0) Unknown

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
