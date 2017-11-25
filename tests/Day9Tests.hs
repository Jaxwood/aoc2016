module Day9Tests where
  import Test.HUnit
  import Day9 (resulta, resultb)

  day9 :: String -> Test
  day9 a = test [
      assertEqual "day9a" 6 (resulta a),
      assertEqual "day9b" 0 (resultb "")
    ]
