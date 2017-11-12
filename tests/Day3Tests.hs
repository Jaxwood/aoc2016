module Day3Tests where
  import Test.HUnit
  import Day3 (resulta, resultb)

  day3 :: String -> Test
  day3 day3a = test [
      assertEqual "day3a" 2 (resulta day3a),
      assertEqual "day3b" 3 (resultb day3a) 
    ]

