module Day11Tests where

  import Test.HUnit
  import qualified Data.Map as M
  import Day11 (resulta, resultb)

  day11 :: String -> Test
  day11 a = test [
      assertEqual "day11a" 11 (resulta [["LM","HM"],["HG"],["LG"],[]]),
      assertEqual "day11b" 0 (resultb a)
    ]
