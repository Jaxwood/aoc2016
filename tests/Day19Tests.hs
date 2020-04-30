module Day19Tests where

  import Test.HUnit
  import Day19 (resulta)

  day19 :: Test
  day19 = test [
      assertEqual "day19a" 3 (resulta 5),
      assertEqual "day19a" 1808357 (resulta 3001330)
    ]
