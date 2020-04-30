module Day18Tests where

  import Test.HUnit
  import Day18 (resulta)

  day18 :: Test
  day18 = test [
      assertEqual "day18a" 38 (resulta 10 ".^^.^.^^^^"),
      assertEqual "day18a" 1978 (resulta 40 "^^^^......^...^..^....^^^.^^^.^.^^^^^^..^...^^...^^^.^^....^..^^^.^.^^...^.^...^^.^^^.^^^^.^^.^..^.^"),
      assertEqual "day18b" 20003246 (resulta 400000 "^^^^......^...^..^....^^^.^^^.^.^^^^^^..^...^^...^^^.^^....^..^^^.^.^^...^.^...^^.^^^.^^^^.^^.^..^.^")
    ]
