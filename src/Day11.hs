module Day11 (resulta, RTG(Generator,Microchip)) where
 
  import qualified Data.Map as M
  import qualified Data.Set as S
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
  combinations :: Int -> M.Map Int [RTG] -> Maybe [[RTG]]
  combinations lvl m =
    let combos = map S.toList . S.toList . S.powerSet . S.fromList <$> M.lookup lvl m
    in withinRange <$> combos

  resulta :: M.Map Int [RTG] -> Maybe [[RTG]]
  resulta m = combinations 1 m
