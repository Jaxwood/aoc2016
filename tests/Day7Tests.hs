module Day7Tests where
  import Test.HUnit
  import Data.Either
  import Day7 (resulta, resultb)

  day7 :: String -> Test
  day7 str = test [
      assertEqual "day7a" 2 (resulta str)
      -- assertEqual "day7b" "" (resultb str),
    ]
