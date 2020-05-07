module Day24Tests where

  import Test.HUnit
  import Day24 (resulta)

  day24 :: String -> String -> Test
  day24 a b = test [
      assertEqual "day24a" 14 (resulta b),
      assertEqual "day24a" 428 (resulta a)
    ]
