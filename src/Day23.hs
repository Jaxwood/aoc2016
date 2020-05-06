module Day23 (resulta) where

  import qualified Data.Map as M
 
  data IntOrString = Offset Int | Register String deriving (Eq, Show)
  data Instruction =
    Copy IntOrString String |
    Inc String | Dec String |
    Jump IntOrString IntOrString |
    Toggle String
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
    | x == "jnz" = Jump offset register
    | x == "tgl" = Toggle (head xs)
    where 
      offset = case parseInt (head xs) of
          Nothing -> Register (head xs)
          (Just x) -> Offset x
      register = case parseInt (last xs) of
          Nothing -> Register (last xs)
          (Just x) -> Offset x

  parse :: [String] -> [Instruction]
  parse ls = map (toInstruction . words) ls

  run :: Instruction -> [Instruction] -> Int -> Memory -> (Int, Memory, [Instruction])
  run (Copy (Offset x) y) is idx m = ((idx + 1), M.insert y x m, is)
  run (Copy (Register x) y) is idx m = ((idx + 1), M.insert y (m M.! x) m, is)
  run (Inc x) is idx m = ((idx + 1), M.insert x ((m M.! x) + 1) m, is)
  run (Dec x) is idx m = ((idx + 1), M.insert x ((m M.! x) - 1) m, is)
  run (Jump (Offset x) (Offset y)) is idx m = if x == 0 then ((idx + 1), m, is) else ((idx + y), m, is)
  run (Jump (Offset x) (Register y)) is idx m = if x == 0 then ((idx + 1), m, is) else ((idx + (m M.! y)), m, is)
  run (Jump (Register x) (Offset y)) is idx m = if (m M.! x) == 0 then ((idx + 1), m, is) else ((idx + y), m, is)
  run (Jump (Register x) (Register y)) is idx m = if (m M.! x) == 0 then ((idx + 1), m, is) else ((idx + (m M.! y)), m, is)
  run (Toggle x) is idx m = (idx + 1, m, is')
    where is' = case toggle (Toggle x) is idx m of
                  Nothing -> is
                  (Just is') -> is'

  safeGet :: Int -> [a] -> Maybe a
  safeGet idx lst =
    if idx >= length lst || idx < 0 then Nothing else Just $ lst !! idx

  modify :: Instruction -> Instruction
  modify (Toggle x) = (Inc x)
  modify (Inc x) = (Dec x)
  modify (Copy x y) = (Jump x (Register y))
  modify (Jump x (Register y)) = (Copy x y)
  modify a = a

  merge :: [Instruction] -> Int -> Instruction -> [Instruction]
  merge is idx i = f ++ (i:(drop 1 l))
    where (f,l) = splitAt idx is

  toggle :: Instruction -> [Instruction] -> Int -> Memory -> Maybe [Instruction]
  toggle (Toggle x) is i m = merge is idx <$> modify <$> safeGet idx is
    where rel = m M.! x
          idx = i + rel
  toggle _ _ _ _ = Nothing

  runner :: [Instruction] -> Int -> Memory -> Int
  runner is i m
    | i >= (length is) = m M.! "a"
    | otherwise = runner is' i' m'
    where (i', m', is') = run (is !! i) is i m

  resulta :: String -> Memory -> Int
  resulta ls mem = runner instructions 0 mem
    where
      instructions = (parse . lines) ls
