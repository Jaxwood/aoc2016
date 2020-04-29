module Day15Tests where

  import Test.HUnit
  import Day15 (resulta)

  day15 :: Test
  day15 = test [
      assertEqual "day15a" 5 (resulta [(5,4),(2,1)]),
      assertEqual "day15a" 0 (resulta [(17,15),(3,2),(19,4),(13,2),(7,2),(5,0)])
    ]
