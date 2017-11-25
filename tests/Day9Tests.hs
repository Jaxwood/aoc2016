module Day9Tests where
  import Test.HUnit
  import Day9 (resulta, resultb)

  day9 :: String -> Test
  day9 a = test [
      assertEqual "day9a" "ADVENT" (resulta "ADVENT"),
      assertEqual "day9a" "ABBBBBC" (resulta "A(1x5)BC"),
      assertEqual "day9b" 0 (resultb "")
    ]
