module Day23Tests where

  import Test.HUnit
  import qualified Data.Map as M
  import Day23 (resulta)

  day23 :: String -> Test
  day23 a = test [
      assertEqual "day23a" 10953 (resulta a $ M.fromList [("a", 7), ("b", 0), ("c", 0), ("d", 0)])
    ]
