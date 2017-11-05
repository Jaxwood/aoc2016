module Day1Tests where

  import Test.HUnit
  import Day1 (parseInput, move, moves, Move(R, L))

  day1 :: Test
  day1 = test [
      assertEqual "R2, L3" ["R2", "L3"] (parseInput "R2, L3"),
      assertEqual "R2 R2 R2" ["R2", "R2", "R2"] (parseInput "R2, R2, R2"),
      assertEqual "R5, L5, R5, R3" ["R5", "L5", "R5", "R3"] (parseInput "R5, L5, R5, R3"),
      assertEqual "R5" (R 5) (move "R5"),
      assertEqual "L5" (L 5) (move "L5"),
      assertEqual "L5, R5, L1" [L 5, R 5, L 1] (moves "L5, R5, L1")
    ]
