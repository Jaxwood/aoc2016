module Day4Tests where
  import Test.HUnit
  import Day4 (resulta, resultb, isValid)

  day4 :: String -> Test
  day4 day4a = test [
      assertEqual "day4a" 0 (resulta day4a),
      assertEqual "day4b" 0 (resultb day4a),
      assertEqual "is valid" True (isValid "aaaaabbbzyx" "abxyz"),
      assertEqual "is not valid" False (isValid "totallyrealroom" "decoy")
    ]
