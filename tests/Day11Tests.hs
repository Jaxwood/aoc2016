module Day11Tests where

  import Test.HUnit
  import qualified Data.Map as M
  import Day11 (resulta, RTG(Generator,Microchip))

  day11 :: String -> Test
  day11 a = test [
      assertEqual "day11a" 11 (resulta $ M.fromList [
        (1, [Microchip 'L', Microchip 'H']),
        (2, [Generator 'H']),
        (3, [Generator 'L']),
        (4, [])])
    ]
