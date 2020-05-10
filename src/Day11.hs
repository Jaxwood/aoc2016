module Day11 (resulta, RTG(..), State(..)) where
 
  import qualified Data.Sequence as Q
  import qualified Data.Map as M
  import qualified Data.Set as S
  import Data.List

  data RTG = Generator Char | Microchip Char deriving (Eq,Show)

  type Building = M.Map Int [RTG]

  -- Make RTG instance of Ord
  instance Ord RTG where
    (Generator x) `compare` (Generator y) = x `compare` y
    (Microchip x) `compare` (Microchip y) = x `compare` y
    (Generator x) `compare` (Microchip y) = GT
    (Microchip x) `compare` (Generator y) = LT

  -- Represent a snapshot of the building
  data State = State {
    current :: Int,
    moves :: Int,
    building :: Building
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

  -- find next levels to move to
  levels :: Int -> [Int]
  levels l
    |  l == 1 = [2]
    |  l == 2 = [1,3]
    |  l == 3 = [2,4]
    |  l == 4 = [3]
  
  -- update the building layout
  layout :: State -> (Int, [RTG]) -> State
  layout s (l, rts) = s { current = l, moves = succ m, building = final }
    where lvl = current s
          b = building s
          m = moves s
          next = b M.! l
          cur = b M.! lvl
          update = M.insert lvl (sort (cur \\ rts)) b
          final = M.insert l (sort (next ++ rts)) update

  -- move the elevator
  move :: State -> [RTG] -> [State]
  move s rts = map (layout s) pairs
    where lvls = levels $ current s
          pairs = map (\l -> (l, rts)) lvls

  -- check if this layout has been seen before
  notSeenBefore :: S.Set (Int, Building) -> State -> Bool
  notSeenBefore v s = S.notMember (current s, building s) v

  -- keep generating new states until every RTG is on the top floor
  resulta :: Q.Seq State -> Int -> S.Set (Int, Building) -> Int
  resulta ss until v = if done then (moves s) else resulta ((Q.><) (Q.drop 1 ss) (Q.fromList next)) until v'
    where s = Q.index ss 0
          b = building s
          c = current s
          arrangements = combinations $ b M.! c
          next = filter (notSeenBefore v) $ filter validate $ concatMap (move s) arrangements
          done = until == length (b M.! 4)
          v' = S.union (S.fromList $ map (\s -> (current s, building s)) next) v
