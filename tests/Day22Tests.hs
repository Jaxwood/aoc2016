module Day22Tests where

  import Test.HUnit
  import qualified Data.Map as M
  import Day22 (resulta, resultb, Node(..), State(..))

  day22 :: String -> String -> Test
  day22 a b = test [
      assertEqual "day22a" 1003 (resulta a),
      assertEqual "day22b" 7 (resultb b)
      -- assertEqual "day22b" 192 (resultb a)
    ]
