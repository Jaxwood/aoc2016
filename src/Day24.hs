module Day24 (resulta) where

  import qualified Data.Map as M

  data Location = Wall | Open | Blueprint Char deriving (Eq,Show)

  type Grid = M.Map (Int, Int) Location

  location :: Char -> Location
  location c
    | c == '#' = Wall
    | c == '.' = Open
    | otherwise = Blueprint c

  line :: String -> Int -> [((Int, Int), Location)]
  line ss y = zipWith (\l x -> ((x,y), location l)) ss [0..]

  parse :: String -> Grid
  parse s = M.fromList coords
    where lns = lines s
          coords = concat $ zipWith line lns [0..] 

  resulta :: String -> Grid
  resulta a = grid
    where grid = parse a
