module Day06Tests where
  import Test.HUnit
  import Day06 (resulta, resultb)

  day6 :: String -> Test
  day6 str = test [
      assertEqual "day6a" "easter" (resulta str),
      assertEqual "day6b" "advent" (resultb str)
    ]
