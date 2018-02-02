module Day11Tests where

  import Test.HUnit
  import qualified Data.Map as M
  import Day11 (resulta, resultb, RTG(Generator,Microchip),validate)

  day11 :: String -> Test
  day11 a = test [
      assertEqual "day11a" 11 (resulta [[Microchip 'L', Microchip 'H'],[Generator 'H'],[Generator 'L'],[]]),
      assertEqual "day11b" 0 (resultb a),
      assertEqual "validate" True (validate (Microchip 'L') (Microchip 'H')),
      assertEqual "validate" True (validate (Microchip 'L') (Generator 'L')),
      assertEqual "validate" True (validate (Microchip 'H') (Generator 'H')),
      assertEqual "validate" False (validate (Generator 'L') (Microchip 'H')),
      assertEqual "validate" False (validate (Generator 'H') (Microchip 'L'))
    ]
