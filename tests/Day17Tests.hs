module Day17Tests where

  import Test.HUnit
  import Day17 (resulta,resultb)

  day17 :: Test
  day17 = test [
      assertEqual "day17a" "DDRRRD" (resulta ([Just ((0,0), "ihgpwlah")])),
      assertEqual "day17a" "DDUDRLRRUDRD" (resulta ([Just ((0,0), "kglvqrro")])),
      assertEqual "day17a" "DRURDRUDDLLDLUURRDULRLDUUDDDRR" (resulta ([Just ((0,0), "ulqzkmiv")])),
      assertEqual "day17a" "RDRLDRDURD" (resulta ([Just ((0,0), "pgflpeqp")])),
      assertEqual "day17b" 370 (resultb [] [Just ((0,0), "ihgpwlah")]),
      assertEqual "day17b" 492 (resultb [] [Just ((0,0), "kglvqrro")]),
      assertEqual "day17b" 830 (resultb [] [Just ((0,0), "ulqzkmiv")]),
      assertEqual "day17b" 596 (resultb [] [Just ((0,0), "pgflpeqp")])
    ]
