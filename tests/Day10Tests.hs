module Day10Tests where

  import Test.HUnit
  import qualified Data.Map as M
  import Day10 (resulta, resultb)

  day10 :: String -> Test
  day10 a = test [
      assertEqual "day10a" 2 (resulta a 2 5),
      assertEqual "day10b" 30 (resultb a 2 5)
    ]
