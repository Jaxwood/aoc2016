module Day22Tests where

  import Test.HUnit
  import Day22 (resulta)

  day22 :: String -> Test
  day22 a = test [
      assertEqual "day22a" 1003 (resulta a)
    ]
