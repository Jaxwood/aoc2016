module Day20Tests where

  import Test.HUnit
  import Day20 (resulta)

  day20 :: String -> Test
  day20 a = test [
      assertEqual "day20a" 14975795 (resulta a)
    ]
