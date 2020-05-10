module Day11Tests where

  import Test.HUnit
  import qualified Data.Map as M
  import qualified Data.Sequence as Q
  import qualified Data.Set as S
  import Day11 (resulta, RTG(Generator,Microchip), State(..))

  day11 :: String -> Test
  day11 a = test [
      assertEqual "day11a" 33 (resulta (Q.singleton (Day11.State {
        current = 1,
        moves = 0,
        building = M.fromList [
          (1, [Microchip 'A', Generator 'A']),
          (2, [Generator 'B', Generator 'C', Generator 'D', Generator 'E']),
          (3, [Microchip 'B', Microchip 'C', Microchip 'D', Microchip 'E']),
          (4, [])]
        })) 10 (S.insert (1, M.fromList [
          (1, [Microchip 'A', Generator 'A']),
          (2, [Generator 'B', Generator 'C', Generator 'D', Generator 'E']),
          (3, [Microchip 'B', Microchip 'C', Microchip 'D', Microchip 'E']),
          (4, [])]) S.empty)),
      assertEqual "day11a" 57 (resulta (Q.singleton (Day11.State {
        current = 1,
        moves = 0,
        building = M.fromList [
          (1, [Microchip 'A', Microchip 'F', Microchip 'G', Generator 'A', Generator 'F', Generator 'G']),
          (2, [Generator 'B', Generator 'C', Generator 'D', Generator 'E']),
          (3, [Microchip 'B', Microchip 'C', Microchip 'D', Microchip 'E']),
          (4, [])]
        })) 14 (S.insert (1, M.fromList [
          (1, [Microchip 'A', Microchip 'F', Microchip 'G', Generator 'A', Generator 'F', Generator 'G']),
          (2, [Generator 'B', Generator 'C', Generator 'D', Generator 'E']),
          (3, [Microchip 'B', Microchip 'C', Microchip 'D', Microchip 'E']),
          (4, [])]) S.empty))
    ]
