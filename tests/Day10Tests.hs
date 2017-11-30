module Day10Tests where

  import Test.HUnit
  import Day10 (resulta, resultb)

  day10 :: String -> Test
  day10 a = test [
      assertEqual "day10a" 2 (resulta a [5,2]),
      assertEqual "day10b" "" (resultb "")
    ]
