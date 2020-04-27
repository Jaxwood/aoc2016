module Day12 (resulta) where

  import qualified Data.Map as M
 
  data IntOrString = Offset Int | Register String deriving (Eq, Show)
  data Instruction =
    Copy IntOrString String |
    Inc String | Dec String |
    Jump IntOrString Int
    deriving (Show, Eq)

  type Memory = M.Map String Int

  parseInt :: String -> Maybe Int
  parseInt candidate =
    case reads candidate :: [(Int, String)] of
      [] -> Nothing
      [(i, _)] -> Just i
    
  toInstruction :: [String] -> Instruction
  toInstruction (x:xs)
    | x == "cpy" = Copy offset (last xs)
    | x == "inc" = Inc (head xs)
    | x == "dec" = Dec (head xs)
    | x == "jnz" = Jump offset (read (last xs) :: Int)
    where 
      offset = case parseInt (head xs) of
          Nothing -> Register (head xs)
          (Just x) -> Offset x

  parse :: [String] -> [Instruction]
  parse ls = map (toInstruction . words) ls

  run :: Instruction -> Int -> Memory -> (Int, Memory)
  run (Copy (Offset x) y) idx m = ((idx + 1), M.insert y x m)
  run (Copy (Register x) y) idx m = ((idx + 1), M.insert y (m M.! x) m)
  run (Inc x) idx m = ((idx + 1), M.insert x ((m M.! x) + 1) m)
  run (Dec x) idx m = ((idx + 1), M.insert x ((m M.! x) - 1) m)
  run (Jump (Offset x) y) idx m = if x == 0 then ((idx + 1), m) else ((idx + y), m)
  run (Jump (Register x) y) idx m = if (m M.! x) == 0 then ((idx + 1), m) else ((idx + y), m)

  runner :: [Instruction] -> Int -> Memory -> Int
  runner is i m
    | i >= (length is) = m M.! "a"
    | otherwise = runner is i' m'
      where (i', m') = run (is !! i) i m

  resulta :: String -> Memory -> Int
  resulta ls mem = runner instructions 0 mem
    where
      instructions = (parse . lines) ls
