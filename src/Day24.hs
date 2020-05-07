module Day24 (resulta) where

  import Data.List
  import qualified Data.Map as M
  import qualified Data.Set as S

  data Location = Wall | Open | Blueprint Char deriving (Eq,Show)

  type Grid = M.Map (Int, Int) Location
  type Memory = M.Map Char Int
  type Global = M.Map Char Memory
  type Coord = ((Int,Int), Int)

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
  
  neighbors :: Grid -> S.Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
  neighbors g v (x,y) = filter (\a -> S.notMember a v) $ filter (\c -> (g M.! c) /= Wall) $ map (\(x',y') -> (x+x',y+y')) [(0,1), (1,0), (-1,0), (0,-1)]
  
  scan :: Grid -> S.Set (Int, Int) -> [Coord] -> Memory -> Memory
  scan _ _ [] acc = acc
  scan g v ((s,m):ss) acc = if S.member s v then scan g v ss acc else scan g v' (ss ++ next') acc'
    where next = neighbors g v s
          v' = S.insert s v
          acc' = case g M.! s of
            (Blueprint b) -> M.insert b m acc
            _ -> acc
          next' = map (\n -> (n, succ m)) next

  distanceTo :: Global -> Char -> Char -> Int
  distanceTo g a b = a' M.! b
    where a' = g M.! a
  
  distances :: Global -> [String] -> [Int] -> Int
  distances _ [] acc = minimum acc
  distances g (s:ss) acc = distances g ss ((fst res):acc)
    where res = foldl' (\(a, prev) n -> (a + distanceTo g prev n, n)) (0, head s) (tail s)
  
  getLocation :: Location -> Char
  getLocation (Blueprint c) = c

  resulta :: String -> Int
  resulta s = distances mem opt []
    where g = parse s
          ks = M.toList $ M.filter (\c -> c /= Wall && c /= Open) g
          opt = filter (\xs -> head xs == '0') $ permutations $ map (getLocation . snd) ks
          mem = foldr (\n acc -> case (g M.! n) of (Blueprint c) -> M.insert c (scan g S.empty [(n,0)] M.empty) acc) M.empty (map fst ks)
