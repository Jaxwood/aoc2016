module Day21 (resulta) where

  data Direction = L | R deriving (Eq,Show)

  data Instruction =
    Move Int Int |
    RotateLeftRight Direction Int |
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

  rotateLeftRight :: Instruction -> String -> String
  rotateLeftRight (RotateLeftRight d steps) candidate
    | d == L = take len $ drop steps $ cycle candidate
    | d == R = take len $ drop (len - steps) $ cycle candidate
    where len = length candidate

  move :: Instruction -> String -> String
  move (Move x y) candidate =
    f ++ m ++ x':b
    where f = take x candidate
          m = take (y - x) $ drop (succ x) candidate
          b = drop (succ y) candidate
          x' = candidate !! x
          
  resulta :: String -> String -> String
  resulta input password =
    move (Move 1 4) $
    rotateLeftRight (RotateLeftRight L 1) $
    reversePosition (Reverse 0 4) $
    swapLetter (SwapLetter 'd' 'b') $
    swapPosition (SwapPosition 0 4) password
