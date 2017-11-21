module Day6Tests where
  import Test.HUnit
  import Day6 (resulta, resultb)

  day6 :: String -> Test
  day6 str = test [
      assertEqual "day6a" "" (resulta "abc"),
      assertEqual "day6b" "" (resultb "abc")
    ]
