module Day21 (resulta) where

  import Data.List (findIndex)
  import Data.Maybe

  data Direction = L | R deriving (Eq,Show)

  data Instruction =
    Rotate Char |
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
    | d == R = take len $ drop (len' - steps) $ cycle candidate
    where len = length candidate
          len' = if len > steps then len else len * (steps `div` len + 1)

  move :: Instruction -> String -> String
  move (Move x y) candidate = snd $
    foldr (\c (i, acc) ->
      if x == i then (pred i, acc)
      else if y == i then (pred i, if x < y then c:x':acc else x':c:acc)
      else (pred i, c:acc)) (len, "") candidate
    where x' = candidate !! x
          len = length candidate - 1

  rotate :: Instruction -> String -> String
  rotate (Rotate x) candidate = 
    rotateLeftRight (RotateLeftRight R idx') candidate
    where idx = fromJust $ findIndex (==x) candidate
          idx' = if idx >= 4 then idx + 2 else idx + 1
          
  resulta :: String -> String -> String
  resulta input password =
    rotate (Rotate 'd') $
    rotate (Rotate 'b') $
    move (Move 3 0) $
    move (Move 1 4) $
    rotateLeftRight (RotateLeftRight L 1) $
    reversePosition (Reverse 0 4) $
    swapLetter (SwapLetter 'd' 'b') $
    swapPosition (SwapPosition 0 4) password
