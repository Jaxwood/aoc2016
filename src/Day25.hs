module Day25 (resulta) where

  import qualified Data.Map as M
 
  data IntOrString = Offset Int | Register String deriving (Eq, Show)
  data Instruction =
    Copy IntOrString String |
    Inc String | Dec String |
    Jump IntOrString IntOrString |
    Toggle String |
    Out String
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
    | x == "out" = Out (head xs)
    where 
      offset = case parseInt (head xs) of
          Nothing -> Register (head xs)
          (Just x) -> Offset x
      register = case parseInt (last xs) of
          Nothing -> Register (last xs)
          (Just x) -> Offset x

  parse :: [String] -> [Instruction]
  parse ls = map (toInstruction . words) ls

  run :: Instruction -> [Instruction] -> Int -> Memory -> [Int] -> (Int, Memory, [Instruction], [Int])
  run (Copy (Offset x) y) is idx m acc = ((idx + 1), M.insert y x m, is, acc)
  run (Copy (Register x) y) is idx m acc = ((idx + 1), M.insert y (m M.! x) m, is, acc)
  run (Inc x) is idx m acc = ((idx + 1), M.insert x ((m M.! x) + 1) m, is, acc)
  run (Dec x) is idx m acc = ((idx + 1), M.insert x ((m M.! x) - 1) m, is, acc)
  run (Jump (Offset x) (Offset y)) is idx m acc = if x == 0 then ((idx + 1), m, is, acc) else ((idx + y), m, is, acc)
  run (Jump (Offset x) (Register y)) is idx m acc = if x == 0 then ((idx + 1), m, is, acc) else ((idx + (m M.! y)), m, is, acc)
  run (Jump (Register x) (Offset y)) is idx m acc = if (m M.! x) == 0 then ((idx + 1), m, is, acc) else ((idx + y), m, is, acc)
  run (Jump (Register x) (Register y)) is idx m acc = if (m M.! x) == 0 then ((idx + 1), m, is, acc) else ((idx + (m M.! y)), m, is, acc)
  run (Out x) is idx m acc = (idx + 1, m, is, (m M.! x):acc)
  run (Toggle x) is idx m acc = (idx + 1, m, is', acc)
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

  runner :: [Instruction] -> Int -> Memory -> [Int] -> [Int]
  runner is i m acc
    | length acc == 10 = (reverse acc)
    | otherwise = runner is' i' m' acc'
    where (i', m', is', acc') = run (is !! i) is i m acc

  resulta :: String -> Int -> Int
  resulta ls num = if res == [0,1,0,1,0,1,0,1,0,1] then num else resulta ls (succ num)
    where
      res = runner instructions 0 mem []
      mem = M.fromList [("a", num), ("b", 0), ("c", 0), ("d", 0)]
      instructions = (parse . lines) ls
