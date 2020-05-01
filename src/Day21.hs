module Day21 (resulta) where

  data Instruction =
    Reverse Int Int |
    SwapPosition Int Int |
    SwapLetter Char Char deriving (Eq,Show)

  swapPosition :: Instruction -> String -> String
  swapPosition (SwapPosition x y) candidate =
    zipWith swapper candidate [0..]
    where swapper = (\c i -> if i == x then y' else if i == y then x' else c)
          x' = candidate !! x
          y' = candidate !! y

  swapLetter :: Instruction -> String -> String
  swapLetter (SwapLetter x y) candidate =
    map swapper candidate
    where swapper = (\c -> if c == x then y else if c == y then x else c)
  
  reversePosition :: Instruction -> String -> String
  reversePosition (Reverse x y) candidate =
    f ++ (reverse $ take (succ y) $ drop x candidate) ++ b
    where f = take x candidate
          b = drop (succ y) candidate

  resulta :: String -> String -> String
  resulta input password =
    reversePosition (Reverse 0 4) $
    swapLetter (SwapLetter 'd' 'b') $
    swapPosition (SwapPosition 0 4) password
