module Day25Tests where

  import Test.HUnit
  import Day25 (resulta)

  day25 :: String -> Test
  day25 a = test [
      assertEqual "day25a" 198 (resulta a 0)
    ]
