module Day24 (resulta) where

  import qualified Data.Map as M
  import qualified Data.Set as S

  data Location = Wall | Open | Blueprint Char deriving (Eq,Show)

  type Grid = M.Map (Int, Int) Location

  data State = State {
    current :: (Int, Int),
    moves :: Int,
    visited :: S.Set (Int, Int),
    blueprints :: S.Set Char
  }

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
  
  neighbors :: State -> Grid -> (Int, Int) -> [(Int, Int)]
  neighbors s g (x,y) = filter (\a -> S.notMember a visit) $ filter (\c -> (g M.! c) /= Wall) $ map (\(x',y') -> (x+x',y+y')) [(0,1), (1,0), (-1,0), (0,-1)]
    where visit = visited s

  toState :: Grid -> State -> (Int, Int) -> State
  toState g s c = State { current = c, moves = succ m, visited = voc, blueprints = loc }
    where m = moves s
          v = visited s
          b = blueprints s
          voc = case g M.! c of
            Open -> S.insert c v
            (Blueprint v) -> S.singleton c
          loc = case g M.! c of
            Open -> b
            (Blueprint blue) -> S.insert blue b

  travel :: Grid -> [State] -> [Int] -> Int
  travel _ [] acc = minimum acc
  travel g (s:ss) acc = if length (blueprints s) == 4 then (moves s) else travel g (ss ++ ss') acc'
    where next = neighbors s g $ current s
          ss' = map (toState g s) next
          acc' = if length (blueprints s) == 4 then (moves s):acc else acc

  resulta :: String -> Int
  resulta a = travel grid' [state] [] -- neighbors state grid' start
    where grid = parse a
          grid'= M.insert start Open grid
          start = fst $ head $ M.toList $ M.filter (== Blueprint '0') grid
          state = State { current = start, moves = 0, visited = S.empty, blueprints = S.empty }
