module Day4Tests where
  import Test.HUnit
  import Day4 (resulta, resultb)

  day4 :: String -> Test
  day4 day4a = test [
      assertEqual "day4a" 1514 (resulta day4a),
      assertEqual "day4b" 343 (resultb "qzmt-zixmtkozy-ivhz-343[abcde]" "very encrypted name")
    ]
