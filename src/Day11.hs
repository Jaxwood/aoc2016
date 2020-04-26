module Day11 (resulta, RTG(Generator,Microchip)) where
 
  import qualified Data.Map as M
  import qualified Data.Set as S
  import Data.List
  import Data.Maybe

  data RTG = Generator Char | Microchip Char deriving (Eq,Show)

  instance Ord RTG where
    (Generator x) `compare` (Generator y) = x `compare` y
    (Microchip x) `compare` (Microchip y) = x `compare` y
    (Microchip x) `compare` (Generator y) = x `compare` y
    (Generator x) `compare` (Microchip y) = x `compare` y

  -- only combinations of length (1|2) is valid
  withinRange :: [[RTG]] -> [[RTG]]
  withinRange = filter ((\x -> (x > 0) && (x < 3)) . length) 

  -- get all the combinations for microchip and generator on a certain building floor
  combinations :: M.Map Int [RTG] -> Int -> Maybe [[RTG]]
  combinations m lvl =
    let combos = map S.toList . S.toList . S.powerSet . S.fromList <$> M.lookup lvl m
    in withinRange <$> combos

  -- move the elevator
  move :: M.Map Int [RTG] -> Int -> [RTG] -> [M.Map Int [RTG]]
  move m lvl rs
    | lvl == 1 = [M.update (const addedAbove) (lvl + 1) (M.update (const removed) lvl m)]
    | lvl == 4 =[M.update (const addedBelow) (lvl - 1) (M.update (const removed) lvl m)]
    | otherwise = [
        M.update (const addedAbove) (lvl + 1) (M.update (const removed) lvl m),
        M.update (const addedBelow) (lvl - 1) (M.update (const removed) lvl m)
      ]
    where current = M.lookup lvl m
          above = M.lookup (lvl + 1) m
          below = M.lookup (lvl - 1) m
          removed = (\\ rs) <$> current
          addedAbove = (++ rs) <$> above
          addedBelow = (++ rs) <$> below

  -- create new maps based on the combinations
  toMap :: M.Map Int [RTG] -> Int -> [[RTG]] -> [M.Map Int [RTG]]
  toMap m lvl cs = concatMap (move m lvl) cs

  resulta :: M.Map Int [RTG] -> Int -> Maybe [M.Map Int [RTG]]
  resulta m lvl = toMap m lvl <$> combinations m lvl 
