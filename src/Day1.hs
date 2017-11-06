module Day1 (parseInput, move, moves, Move(R, L)) where

  import Data.List.Split
  import qualified Data.Text as T

  data Move = R Int | L Int deriving (Eq, Show)
  data Position = Position Int Int
  data Coord = Coord Position Move

  result :: String -> Int
  result = blocks . calculate . moves

  blocks :: Coord -> Int
  blocks (Coord (Position x y) _) =
    x + y
  
  calculate :: [Move] -> Coord
  calculate (x:xs) = foldl acc (Coord (Position 0 0) x) xs

  acc :: Coord -> Move -> Coord
  acc coord move =
    Coord (Position 0 0) move

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
