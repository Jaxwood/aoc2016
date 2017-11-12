module Day2Tests where

  import Test.HUnit
  import Day2 (resulta)

  day2 :: Test
  day2 = test [
      assertEqual "ULL\nRRDDD\nLURDL\nUUUUD" 1985 (resulta "ULL\nRRDDD\nLURDL\nUUUUD")
    ]
