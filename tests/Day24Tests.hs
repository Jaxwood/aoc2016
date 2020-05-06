module Day24Tests where

  import Test.HUnit
  import qualified Data.Map as M
  import Day24 (resulta)

  day24 :: String -> Test
  day24 a = test [
      assertEqual "day24a" M.empty (resulta a)
    ]
