module Day5Tests where
  import Test.HUnit
  import Day5 (resulta, resultb)

  day5 :: Test
  day5 = test [
      assertEqual "day5a" "18f47a30" (resulta "abc"),
      assertEqual "day5b" 0 (resultb "")
    ]
