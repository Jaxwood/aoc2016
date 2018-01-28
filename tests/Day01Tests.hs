module Day01Tests where

  import Test.HUnit
  import Day01 (resulta, resultb)

  day1 :: Test
  day1 = test [
      assertEqual "R2, L3" 5 (resulta "R2, L3"),
      assertEqual "R2 R2 R2" 2 (resulta "R2, R2, R2"),
      assertEqual "R5, L5, R5, R3" 12 (resulta "R5, L5, R5, R3"),
      assertEqual "R8, R4, R4, R8" 4 (resultb "R8, R4, R4, R8")
    ]
