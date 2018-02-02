module Day11 (resulta, resultb, RTG(Generator,Microchip), validate) where
 
  data RTG = Generator Char | Microchip Char deriving (Eq,Show)

  resulta :: [[RTG]] -> Int
  resulta = length

  resultb :: String -> Int
  resultb s = 0

  validate :: RTG -> RTG -> Bool
  validate (Generator _) (Generator _) = True
  validate (Microchip x) (Generator y) = x == y
  validate (Generator x) (Microchip y) = x == y
  validate (Microchip _) (Microchip _) = True
