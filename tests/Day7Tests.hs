module Day7Tests where
  import Test.HUnit
  import Data.Either
  import Day7 (resulta, resultb, palindrome, toIPv7, IPv7(IPv7))

  day7 :: String -> Test
  day7 str = test [
      assertEqual "day7a" 2 (resulta str),
      assertEqual "day7b" "" (resultb str),
      assertEqual "palindrome" True (palindrome "ABBA"),
      assertEqual "palindrome" False (palindrome "aaaa"),
      assertEqual "palindrome" True (palindrome "ioxxoj"),
      assertEqual "parse" (Right (IPv7 "abba" "mnop" "qrst")) (toIPv7 "abba[mnop]qrst")
    ]
