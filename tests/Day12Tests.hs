module Day12Tests where

  import Test.HUnit
  import Day12 (resulta)
  import qualified Data.Map as M

  day12 :: String -> Test
  day12 a = test [
      assertEqual "day12a" 318083 (resulta a $ M.fromList [("a", 0), ("b", 0), ("c", 0), ("d", 0)]),
      assertEqual "day12b" 9227737 (resulta a $ M.fromList [("a", 0), ("b", 0), ("c", 1), ("d", 0)])
    ]
