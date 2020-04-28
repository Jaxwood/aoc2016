module Day13Tests where

  import Test.HUnit
  import Day13 (resulta)

  day13 :: Test
  day13 = test [
      assertEqual "day13a" 11 (resulta (7,4) 10),
      assertEqual "day13a" 82 (resulta (31,39) 1362)
    ]
