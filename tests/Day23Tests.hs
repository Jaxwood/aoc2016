module Day23Tests where

  import Test.HUnit
  import qualified Data.Map as M
  import Day23 (resulta)

  day23 :: String -> Test
  day23 a = test [
      assertEqual "day23a" 10953 (resulta a $ M.fromList [("a", 7), ("b", 0), ("c", 0), ("d", 0)])
      -- Part b:
      -- running the code will take too long time..
      -- after analyzing the instructions the equation is (a! + k)
      -- k is determined by the last 7 instructions e.g. 87*73 = 5913
      -- the solution will then be = 12! + 5913 = 479007513
    ]
