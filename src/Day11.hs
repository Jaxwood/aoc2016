module Day11 (resulta, RTG(Elevator,Generator,Microchip), validate, move) where
 
  import qualified Data.Map as M
  import qualified Data.Set as S
  import Data.List
  import Data.Maybe
  import Control.Monad

  data RTG = Elevator | Generator Char | Microchip Char deriving (Eq,Show)

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
  move :: M.Map Int [RTG] -> Int -> [RTG] -> [Maybe (M.Map Int [RTG])]
  move m lvl rs = [up m, down m]
    where
      up = look lvl (\\ rs) >=> look (lvl + 1) (++ rs) >=> look (lvl + 1) (++ [Elevator])
      down = look lvl (\\ rs) >=> look (lvl - 1) (++ rs) >=> look (lvl - 1) (++ [Elevator])
  
  look :: Int -> ([RTG] -> [RTG]) -> M.Map Int [RTG] -> Maybe (M.Map Int [RTG])
  look k f m = let val = f <$> M.lookup k m
               in case val of
                 Nothing -> Nothing
                 (Just _) -> Just $ M.update (const val) k m

  -- create new maps based on the combinations
  toMap :: M.Map Int [RTG] -> Int -> [[RTG]] -> [Maybe (M.Map Int [RTG])]
  toMap m lvl cs = concatMap (move m lvl) cs

  -- validate the building floor
  isValid :: [RTG] -> RTG -> Bool -> Bool
  isValid rtgs (Microchip x) acc = and [acc, or [matching, noGenerators]]
    where
      matching = (Generator x) `elem` rtgs
      noGenerators = not $ any isGenerator rtgs
  isValid rtgs _ acc = acc

  -- validate the building floor
  validate :: Maybe (M.Map Int [RTG]) -> Bool
  validate rs =
    case rs of
      (Just xs) -> M.foldr (\ns a -> foldr (isValid ns) a ns) True xs
      Nothing -> False

  -- check for generator
  isGenerator :: RTG -> Bool
  isGenerator (Generator x) = True
  isGenerator _ = False

  -- check for elevator
  isElevator :: RTG -> Bool
  isElevator Elevator = True
  isElevator _ = False

  resulta :: M.Map Int [RTG] -> [Maybe (M.Map Int [RTG])]
  resulta m = filter validate $ concatMap (toMap withoutElevator lvl) $ combinations withoutElevator lvl 
              where
                lvl = fst $ head $ M.toList $ M.filter (any isElevator) m
                withoutElevator = M.map (filter (not . isElevator)) m
