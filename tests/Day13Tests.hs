module Day13Tests where

  import Test.HUnit
  import Day13 (resulta)

  day13 :: Test
  day13 = test [
      assertEqual "day13a" True (resulta (7,4) 10) -- 1362
    ]
