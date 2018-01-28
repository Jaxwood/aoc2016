module Day05Tests where
  import Test.HUnit
  import Day05 (resulta, resultb)

  day5 :: Test
  day5 = test [
      assertEqual "day5a" "18f47a30" (resulta "abc"),
      assertEqual "day5b" "05ace8e3" (resultb "abc")
    ]
