module Day1 (resulta, resultb) where

  import Data.Maybe
  import Data.List
  import Data.List.Split
  import qualified Data.Text as T

  data Move = R Int | L Int
  data Position = Position Int Int
  data Pole = North | South | East | West
  data Coord = Coord Position Pole

  resulta :: String -> Int
  resulta = blocks . head . calculate . moves

  resultb :: String -> Int
  resultb = blocks' . fromJust . contains . concatMap path . pairs . reverse . calculate . moves

  blocks :: Coord -> Int
  blocks (Coord (Position x y) _) =
    (abs x) + (abs y)

  blocks' :: (Int, Int) -> Int
  blocks' (x, y) =
    (abs x) + (abs y)
  
  contains :: [(Int, Int)] -> Maybe (Int, Int)
  contains (x:tail)
    | elem x tail == False = contains tail
    | elem x tail == True = Just x

  generate :: (Int -> Int) -> (Int -> Bool) -> Int -> [Int]
  generate f p n =
      takeWhile p $ iterate f n
  
  path :: (Coord, Coord) -> [(Int, Int)]
  path ((Coord (Position x y) _), (Coord (Position x' y')  _))
    | x == x' && y > y' = zip (repeat x) $ generate pred (>y') y
    | x == x' && y < y' = zip (repeat x) $ generate succ (<y') y
    | y == y' && x > x' = zip (generate pred (>x') x) $ repeat y
    | otherwise = zip (generate succ (<x') x) $ repeat y

  pairs :: [Coord] -> [(Coord, Coord)]
  pairs p = zip ((Coord (Position 0 0) West):p) p

  calculate :: [Move] -> [Coord]
  calculate = foldl acc []

  acc :: [Coord] -> Move -> [Coord]
  acc [] (L a) =
    [(Coord (Position (0-a) 0) West)]

  acc [] (R a) =
    [(Coord (Position a 0) East)]

  acc lst@((Coord (Position x y) North):xs) (L a) =
    (Coord (Position (x-a) y) West):lst

  acc lst@((Coord (Position x y) North):xs) (R a) =
    (Coord (Position (x+a) y) East):lst

  acc lst@((Coord (Position x y) South):xs) (L a) =
    (Coord (Position (x+a) y) East):lst

  acc lst@((Coord (Position x y) South):xs) (R a) =
    (Coord (Position (x-a) y) West):lst

  acc lst@((Coord (Position x y) East):xs) (R a) =
    (Coord (Position x (y-a)) South):lst

  acc lst@((Coord (Position x y) East):xs) (L a) =
    (Coord (Position x (y+a)) North):lst

  acc lst@((Coord (Position x y) West):xs)(R a) =
    (Coord (Position x (y+a)) North):lst

  acc lst@((Coord (Position x y) West):xs) (L a) =
    (Coord (Position x (y-a)) South):lst

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
