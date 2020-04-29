module Day14Tests where

  import Test.HUnit
  import Day14 (resulta, resultb)

  day14 :: Test
  day14 = test [
      assertEqual "day14a" 22728 (resulta "abc" 64),
      assertEqual "day14a" 15035 (resulta "ihaygndm" 64),
      assertEqual "day14a" 22551 (resultb "abc" 64),
      assertEqual "day14a" 19968 (resultb "ihaygndm" 64)
    ]
