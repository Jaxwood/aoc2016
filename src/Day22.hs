module Day22 (resulta) where

  import Data.List
  import Data.List.Split

  type X = Int
  type Y = Int
  type Size = Int
  type Used = Int
  type Avail = Int
  type Percent = Int

  data Node = Node (X, Y) Size Used Avail Percent deriving (Eq,Show)

  toInt :: String -> Int
  toInt = read

  parse :: String -> Node
  parse s = Node (toInt (drop 1 x), toInt (drop 1 y)) size used avail percent
    where ws = words s
          [_,x,y] = splitOn "-" (ws !! 0)
          size = toInt $ init (ws !! 1)
          used = toInt $ init (ws !! 2)
          avail = toInt $ init (ws !! 3)
          percent = toInt $ init (ws !! 4)
    
  resulta :: String -> Int
  resulta a = length [ (x,y) | x@(Node _ _ _ a _) <- nodes, y@(Node _ _ u _ _) <- nodes, x /= y && u <= a && u > 0]
    where nodes = map parse $ drop 2 $ lines a
