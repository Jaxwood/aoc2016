module Day8Tests where
  import Test.HUnit
  import Day8 (resulta, resultb)

  day8 :: String -> Test
  day8 a = test [
      assertEqual "day8a" 6 (resulta a),
      assertEqual "day8b" 0 (resultb a)
    ]
