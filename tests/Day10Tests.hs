module Day10Tests where

  import Test.HUnit
  import Day10 (resulta, resultb)

  day10 :: String -> Test
  day10 a = test [
      assertEqual "day10a" [] (resulta a),
      assertEqual "day10b" "" (resultb "")
    ]
