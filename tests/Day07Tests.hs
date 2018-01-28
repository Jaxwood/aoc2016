module Day07Tests where
  import Test.HUnit
  import Data.Either
  import Day07 (resulta, resultb)

  day7 :: String -> String -> Test
  day7 a b = test [
      assertEqual "day7a" 2 (resulta a),
      assertEqual "day7b" 3 (resultb b)
    ]
