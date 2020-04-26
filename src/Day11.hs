module Day11 (resulta, RTG(Generator,Microchip), validate, move) where
 
  import qualified Data.Map as M
  import qualified Data.Set as S
  import Data.List
  import Control.Monad as Mo
  import Data.Maybe

  data RTG = Generator Char | Microchip Char deriving (Eq,Show)

  instance Ord RTG where
    (Generator x) `compare` (Generator y) = x `compare` y
    (Microchip x) `compare` (Microchip y) = x `compare` y
    _ `compare` _ = LT

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
  move m lvl rs =
    case [validate current, validate addedAbove, validate addedBelow] of
      [True, True, True] ->
        [ M.update (const addedAbove) (lvl + 1) (M.update (const removed) lvl m),
          M.update (const addedBelow) (lvl - 1) (M.update (const removed) lvl m)]
      [True, True, False] ->
        [M.update (const addedAbove) (lvl + 1) (M.update (const removed) lvl m)]
      [True, False, True] ->
        [M.update (const addedBelow) (lvl - 1) (M.update (const removed) lvl m)]
      otherwise ->
        []
    where current = M.lookup lvl m
          above = M.lookup (lvl + 1) m
          below = M.lookup (lvl - 1) m
          removed = (\\ rs) <$> current
          addedAbove = (++ rs) <$> above
          addedBelow = (++ rs) <$> below

  -- create new maps based on the combinations
  toMap :: M.Map Int [RTG] -> Int -> [[RTG]] -> [M.Map Int [RTG]]
  toMap m lvl cs = concatMap (move m lvl) cs

  -- validate the building floor
  isValid :: [RTG] -> RTG -> Bool -> Bool
  isValid rtgs (Microchip x) acc = and [acc, or [matching, noGenerators]]
    where
      matching = (Generator x) `elem` rtgs
      noGenerators = not $ any isGenerator rtgs
  isValid rtgs _ acc = acc

  -- validate the building floor
  validate :: Maybe [RTG] -> Bool
  validate rtgs =
    case rtgs of
      (Just rs) -> foldr (isValid rs) True rs 
      Nothing -> False

  -- check for generator
  isGenerator :: RTG -> Bool
  isGenerator (Generator x) = True
  isGenerator _ = False

  resulta :: M.Map Int [RTG] -> Int -> Maybe [M.Map Int [RTG]]
  resulta m lvl = toMap m lvl <$> combinations m lvl 
