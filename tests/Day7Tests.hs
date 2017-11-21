module Day7Tests where
  import Test.HUnit
  import Day7 (resulta, resultb, palindrome)

  day7 :: String -> Test
  day7 str = test [
      assertEqual "day7a" "" (resulta str),
      assertEqual "day7b" "" (resultb str),
      assertEqual "palindrome" True (palindrome "ABBA"),
      assertEqual "palindrome" False (palindrome "aaaa"),
      assertEqual "palindrome" True (palindrome "ioxxoj")
    ]
