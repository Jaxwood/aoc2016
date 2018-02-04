module Day11 (resulta, resultb, RTG(Generator,Microchip), validate) where
 
  data RTG = Generator Char | Microchip Char deriving (Eq,Show)

  resulta :: [[RTG]] -> Int
  resulta = length

  resultb :: String -> Int
  resultb s = 0

  validate :: [RTG] -> [RTG] -> Bool
  validate [] _ = True
  validate ((Generator _):rs) rts = and $ [True,validate rs rts]
  validate ((Microchip x):rs) rts = and $ [or [(Generator x) `elem` rts, all (not . isGenerator) rts], validate rs rts]

  isGenerator :: RTG -> Bool
  isGenerator (Microchip x) = False
  isGenerator (Generator x) = True
