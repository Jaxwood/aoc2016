module Day14Tests where

  import Test.HUnit
  import Day14 (resulta)

  day14 :: Test
  day14 = test [
      assertEqual "day14a" [] (resulta "abc" 64) --22728 
      -- assertEqual "day14a" 0 (resulta "ihaygndm" 64)
    ]
