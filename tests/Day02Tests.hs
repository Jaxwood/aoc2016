module Day02Tests where

  import Test.HUnit
  import Day02 (resulta, resultb)

  day2 :: Test
  day2 = test [
      assertEqual "ULL\nRRDDD\nLURDL\nUUUUD" "1985" (resulta "ULL\nRRDDD\nLURDL\nUUUUD"),
      assertEqual "ULL\nRRDDD\nLURDL\nUUUUD" "5DB3" (resultb "ULL\nRRDDD\nLURDL\nUUUUD")
    ]
