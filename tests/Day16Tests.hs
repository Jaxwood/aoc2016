module Day16Tests where

  import Test.HUnit
  import Day16 (resulta)

  day16 :: Test
  day16 = test [
      assertEqual "day16a" "01100" (resulta 20 "10000"),
      assertEqual "day16a" "10010101010011101" (resulta 272 "10001110011110000"),
      assertEqual "day16b" "01100111101101111" (resulta 35651584 "10001110011110000")
    ]
