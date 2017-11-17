module Day4Tests where
  import Test.HUnit
  import Day4 (resulta, resultb, parseInputResult, Room (Room))

  day4 :: String -> Test
  day4 day4a = test [
      assertEqual "day4a" 0 (resulta day4a),
      assertEqual "day4b" 0 (resultb day4a),
      assertEqual "parse input" (Right (Room "abcdefgh" 987 "abcde")) (parseInputResult "a-b-c-d-e-f-g-h-987[abcde]")
    ]
