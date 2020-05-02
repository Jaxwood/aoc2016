module Day21Tests where

  import Test.HUnit
  import Day21 (resulta, resultb)

  day21 :: String -> Test
  day21 a = test [
      assertEqual "day21a" "gbhcefad" (resulta a "abcdefgh"),
      assertEqual "day21b" "gahedfcb" (resultb a "fbgdceah")
    ]
