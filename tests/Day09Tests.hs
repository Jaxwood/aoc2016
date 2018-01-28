module Day09Tests where
  import Test.HUnit
  import Day09 (resulta, resultb)

  day9 :: String -> Test
  day9 a = test [
      assertEqual "day9a" 57 (resulta a),
      assertEqual "day9b" 445 (resultb "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")
    ]
