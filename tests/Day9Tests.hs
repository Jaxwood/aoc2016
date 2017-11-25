module Day9Tests where
  import Test.HUnit
  import Day9 (resulta, resultb)

  day9 :: String -> Test
  day9 a = test [
      assertEqual "day9a" 6 (resulta "ADVENT"),
      assertEqual "day9a" 7 (resulta "A(1x5)BC"),
      assertEqual "day9a" 9 (resulta "(3x3)XYZ"),
      assertEqual "day9a" 11 (resulta "A(2x2)BCD(2x2)EFG"),
      assertEqual "day9a" 6 (resulta "(6x1)(1x3)A"),
      assertEqual "day9a" 18 (resulta "X(8x2)(3x3)ABCY"),
      assertEqual "day9b" 0 (resultb "")
    ]
