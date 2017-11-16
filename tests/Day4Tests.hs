module Day4Tests where
  import Test.HUnit
  import Day4 (resulta, resultb, parseName, parseSector, parseChecksum)

  day4 :: String -> Test
  day4 day4a = test [
      assertEqual "day4a" 0 (resulta day4a),
      assertEqual "day4b" 0 (resultb day4a),
      assertEqual "parse name" (Right "abcdefgh") (parseName "a-b-c-d-e-f-g-h-987[abcde]"),
      assertEqual "parse sector" (Right "987") (parseSector "987[abcde]"),
      assertEqual "parse checksum" (Right "abcde") (parseChecksum "[abcde]")
    ]
