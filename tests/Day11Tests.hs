module Day11Tests where

  import Test.HUnit
  import qualified Data.Map as M
  import qualified Data.Set as S
  import Day11 (resulta, RTG(Generator,Microchip), State(..))

  day11 :: String -> Test
  day11 a = test [
      assertEqual "day11a" 11 (resulta [Day11.State {
        current = 1,
        moves = 0,
        building = M.fromList [(1, [Microchip 'H', Microchip 'L']), (2, [Generator 'H']), (3, [Generator 'L']), (4, [])]
        }] S.empty)
    ]
