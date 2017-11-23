module Day8Tests where
  import Test.HUnit
  import Day8 (resulta, resultb, rotate)

  day8 :: String -> Test
  day8 a = test [
      assertEqual "day8a" 0 (resulta a),
      assertEqual "day8b" 0 (resultb a),
      assertEqual "rotate" [1,0,0] (rotate [0,0,1] 1),
      assertEqual "rotate" [0,1,0] (rotate [0,0,1] 2),
      assertEqual "rotate" [0,0,1] (rotate [0,0,1] 3),
      assertEqual "rotate" [1,0,0] (rotate [0,0,1] 4),
      assertEqual "rotate" [0,1,0] (rotate [0,0,1] 5),
      assertEqual "rotate" [0,0,1] (rotate [0,0,1] 6),
      assertEqual "rotate" [1,0,0] (rotate [0,0,1] 7)
    ]
