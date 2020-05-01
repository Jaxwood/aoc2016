module Day19Tests where

  import Test.HUnit
  import qualified Data.Sequence as S
  import Day19 (resulta, resultb)

  day19 :: Test
  day19 = test [
      assertEqual "day19a" 3 (resulta 5),
      assertEqual "day19a" 1808357 (resulta 3001330),
      assertEqual "day19b" (S.singleton (Just 2)) (resultb 0 5),
      assertEqual "day19b" (S.singleton (Just 1)) (resultb 0 4),
      assertEqual "day19b" (S.singleton (Just 3)) (resultb 0 6),
      assertEqual "day19b" (S.singleton (Just 1407007)) (resultb 0 3001330)
    ]
