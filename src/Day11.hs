module Day11 (resulta, RTG(Generator,Microchip)) where
 
  import Data.Map as M

  data RTG = Generator Char | Microchip Char deriving (Eq,Show)

  resulta :: M.Map Int [RTG] -> Int
  resulta = length
