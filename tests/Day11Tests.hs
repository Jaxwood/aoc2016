module Day11Tests where

  import Test.HUnit
  import Data.Maybe
  import qualified Data.Map as M
  import Day11 (resulta, RTG(Generator,Microchip), validate, move)

  day11 :: String -> Test
  day11 a = test [
      assertEqual "day11a - step 1" [] (resulta (M.fromList [
        (1, [Microchip 'L', Microchip 'H']),
        (2, [Generator 'H']),
        (3, [Generator 'L']),
        (4, [])]) 1),
      assertEqual "day11a - step 2" [] (resulta (M.fromList [
        (1,[Microchip 'L']),
        (2,[Generator 'H', Microchip 'H']),
        (3,[Generator 'L']),
        (4,[])]) 2),
      assertEqual "validate" True (validate (Just (M.fromList [(1, [(Generator 'L'), (Microchip 'L')])])))
      -- assertEqual "validate" False (validate [(Generator 'L'), (Microchip 'H')]),
      -- assertEqual "validate" True (validate [(Microchip 'L'), (Microchip 'H')]),
      -- assertEqual "validate" True (validate [(Generator 'L'), (Generator 'H')]),
      -- assertEqual "validate" True (validate [(Generator 'H'), (Generator 'L'), (Microchip 'H')]),
      -- assertEqual "validate" True (validate [])
    ]
