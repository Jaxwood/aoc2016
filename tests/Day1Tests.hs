module Day1Tests where

  import Test.HUnit
  import Day1 (result)

  day1 :: Test
  day1 = test [
      assertEqual "R2, L3" 5 (result "R2, L3"),
      assertEqual "R2 R2 R2" 2 (result "R2, R2, R2"),
      assertEqual "R5, L5, R5, R3" 12 (result "R5, L5, R5, R3")
    ]
