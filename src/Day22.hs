module Day22 (resulta, resultb, debug, Node(..), State(..)) where

  import Data.List
  import Data.List.Split
  import qualified Data.Map as M
  import qualified Data.Set as S

  type Size = Int
  type Used = Int

  data Node = Node (Int, Int) Size Used Bool deriving (Eq,Show)

  data State = State {
    grid :: M.Map (Int,Int) Node,
    steps :: Int,
    visited :: S.Set (Int,Int)
  } deriving (Show, Eq)

  instance Ord Node where
    (Node c _ _ _) `compare` (Node c' _ _ _) = snd c `compare` snd c'

  instance Ord State where
    a `compare` b = (steps a) `compare` (steps b)

  comparer :: Node -> Node -> Bool
  comparer (Node c _ _ _) (Node c' _ _ _) = snd c == snd c'

  pixel :: Node -> String
  pixel (Node _ s u _) = if u == 0 then "_" else if u >= 100 then "#" else "."

  debug :: String -> String
  debug a = unlines ss
    where nodes = map parse $ drop 2 $ lines a
          grp = groupBy comparer $ sort nodes
          ss = map (\ns -> unwords $ map pixel ns) grp

  toInt :: String -> Int
  toInt = read

  parse :: String -> Node
  parse s = Node (toInt (drop 1 x), toInt (drop 1 y)) size used False
    where ws = words s
          [_,x,y] = splitOn "-" (ws !! 0)
          size = toInt $ init (ws !! 1)
          used = toInt $ init (ws !! 2)
    
  resulta :: String -> Int
  resulta a = length [ (x,y) | x@(Node _ s u _) <- nodes, y@(Node _ _ u' _) <- nodes, x /= y && u' <= (s - u) && u' > 0]
    where nodes = map parse $ drop 2 $ lines a

  outofbound :: (Int, Int) -> Bool
  outofbound (x,y) = x < 0 || y < 0 || x > 2 || y > 2 

  space :: Node -> Node -> Bool
  space n1@(Node _ s u _) n2@(Node _ _ u' _) = (u + u') <= s

  neighbors :: Node -> S.Set (Int,Int) -> [Node] -> [Node]
  neighbors n@(Node (x, y) _ _ _) visited ns = filter (space n) $ map (\c -> m M.! c) candidates
    where candidates = filter notvisited $ filter (not . outofbound) $ map (\(x', y') -> (x+x',y+y')) [(0,1),(1,0),(-1,0),(0,-1)]
          m = M.fromList $ map (\n@(Node c _ _ _) -> (c, n)) ns
          notvisited = (flip S.notMember) visited

  move :: Node -> Node -> M.Map (Int,Int) Node -> M.Map (Int,Int) Node
  move n1@(Node c s u b) n2@(Node c' s' u' b') m = M.insert c' (Node c' s' u b) from
    where from = M.insert c (Node c s u' b') m
  
  next :: State -> Node -> [Node] -> [State]
  next state n@(Node n' _ _ _) ns = map (\g -> state {grid = g, steps = m, visited = v}) ms
    where ms = map (\neighbor -> move n neighbor (grid state)) ns
          m = succ $ steps state
          v = S.insert n' (visited state)
  
  run :: [State] -> Int
  run [] = 0
  run (state:ss) = if done then (steps state) else run (sort (ss ++ ns))
    where nodes = M.elems (grid state)
          done = any (\(Node c _ _ b) -> c == (0,0) && b) nodes
          start@(Node c _ _ b) = head $ filter (\(Node _ _ u _) -> u == 0) nodes
          v = if c == (2,0) then S.singleton (2,0) else (visited state)
          adjecent = neighbors start v nodes
          ns = next (state {visited = v}) start adjecent

  gridify :: [Node] -> M.Map (Int,Int) Node
  gridify ns = foldr (\v@(Node k _ _ _) acc -> M.insert k v acc) M.empty ns

  resultb :: String -> Int
  resultb a = run [initial]
    where nodes = map (\n@(Node c s u _) -> if c == (2,0) then (Node c s u True) else n) $ map parse $ drop 2 $ lines a
          s@(Node c _ _ _) = head $ filter (\(Node _ _ u _) -> u == 0) nodes
          initial = State { grid = gridify nodes, visited = S.singleton c, steps = 0 }
