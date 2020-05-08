module Day11 (resulta, RTG(..), State(..)) where
 
  import qualified Data.Map as M
  import qualified Data.Set as S
  import Data.List

  data RTG = Generator Char | Microchip Char deriving (Eq,Show)

  instance Ord RTG where
    (Generator x) `compare` (Generator y) = x `compare` y
    (Microchip x) `compare` (Microchip y) = x `compare` y
    _ `compare` _ = LT

  data State = State {
    current :: Int,
    moves :: Int,
    building :: M.Map Int [RTG]
  } deriving (Eq, Show)

   -- get all the combinations for microchip and generator on a certain building floor
  combinations :: [RTG] -> [[RTG]]
  combinations m = filter (\xs -> length xs > 0 && length xs < 3) $ map S.toList $ S.toList $ S.powerSet $ S.fromList m

  -- validate the building floor
  isValid :: [RTG] -> RTG -> Bool -> Bool
  isValid rtgs (Microchip x) acc = and [acc, or [matching, noGenerators]]
    where
      matching = (Generator x) `elem` rtgs
      noGenerators = not $ any isGenerator rtgs
  isValid rtgs _ acc = acc

  -- validate the building floor
  validate :: State -> Bool
  validate s = M.foldr (\ns a -> foldr (isValid ns) a ns) True (building s)

  -- check for generator
  isGenerator :: RTG -> Bool
  isGenerator (Generator x) = True
  isGenerator _ = False

  levels :: Int -> [Int]
  levels l
    |  l == 1 = [2]
    |  l == 2 = [1,3]
    |  l == 3 = [2,4]
    |  l == 4 = [3]
  
  updateBuilding :: State -> (Int, [RTG]) -> State
  updateBuilding s (l, rts) = s { current = l, moves = succ m, building = final }
    where lvl = current s
          b = building s
          m = moves s
          next = b M.! l
          cur = b M.! lvl
          update = M.insert lvl (sort (cur \\ rts)) b
          final = M.insert l (sort (next ++ rts)) update

  move :: State -> [RTG] -> [State]
  move s rts = map (updateBuilding s) pairs
    where lvls = levels $ current s
          pairs = map (\l -> (l, rts)) lvls

  resulta :: [State] -> S.Set (M.Map Int [RTG]) -> Int
  resulta [] _ = 0
  resulta (s:ss) v = if done then (moves s) else resulta (ss ++ next) v'
    where b = building s
          c = current s
          v' = S.insert b v
          combs = combinations $ b M.! c
          next = filter (\m -> S.notMember (building m) v') $ filter validate $ concatMap (move s) combs
          done = 4 == length (b M.! 4)
