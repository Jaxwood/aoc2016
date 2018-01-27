module Day10 (resulta, resultb) where

  import Data.List
  import qualified Data.Map.Strict as M
  import Text.Parsec
  import Text.Parsec.String

  data Instruction = Value Int Int | Instruction Int Target Target deriving (Show,Eq)
  data Target = Bot Int | Output Int deriving (Show,Eq)

  resulta :: String -> Int -> Int -> Int -- M.Map Int [Int]
  resulta s a b = let is = map (right . parseInput) $ lines s
                      m = hydrate M.empty is
                  in follow (\a' b' -> if min a' b' == min a b && max a' b' == max a b then True else False) is m

  resultb :: String -> String
  resultb = id

  follow :: (Int -> Int -> Bool) -> [Instruction] -> M.Map Int [Int] -> Int
  follow fn is m = let m' = M.filter ((==2) . length) m
                       (b,l:h:[]) = if M.null m' then error "not found" else M.elemAt 0 m'
                       b' = find (findInstruction b) is
                   in case b' of
                     Nothing -> error "not found"
                     (Just (Instruction _ t t')) ->
                      if fn l h then b else follow fn is $ remove' b $ update' t' (max l h) $ update' t (min l h) m

  remove' :: Int -> M.Map Int [Int] -> M.Map Int [Int]
  remove' b m = M.adjust (\v -> []) b m

  update' :: Target -> Int -> M.Map Int [Int] -> M.Map Int [Int]
  update' (Output _) i m = m
  update' (Bot b) i m = M.alter (set i) b m

  hydrate :: M.Map Int [Int] -> [Instruction] -> M.Map Int [Int]
  hydrate m [] = m
  hydrate m ((Value f t):is) = hydrate (M.alter (set f) t m) is
  hydrate m (_:is) = hydrate m is

  findInstruction :: Int -> Instruction -> Bool
  findInstruction i (Value _ _) = False
  findInstruction i (Instruction b _ _) = b == i

  set :: Int -> Maybe [Int] -> Maybe [Int]
  set i is = case is of 
               Nothing -> Just [i]
               Just is' -> Just $ i:is'

  -- utility

  right :: Either ParseError a -> a
  right (Left e) = error $ show e
  right (Right a) = a

  -- parse

  parseInput :: String -> Either ParseError Instruction
  parseInput = parse (choice [try parseValue, parseInstruction]) ""

  parseValue :: Parser Instruction
  parseValue = do
    _ <- string "value "
    a <- many1 digit
    _ <- string " goes to bot "
    b <- many1 digit
    return $ Value (read a) (read b)

  parseInstruction :: Parser Instruction
  parseInstruction = do
    _ <- string "bot "
    a <- many1 digit
    _ <- string " gives low to "
    l <- choice [try $ string "bot", string "output"]
    _ <- space
    lv <- many1 digit
    _ <- string " and high to "
    h <- choice [try $ string "bot", string "output"]
    _ <- space
    hv <- many1 digit
    return $ Instruction (read a) (if l == "bot" then Bot (read lv) else Output (read lv)) (if h == "bot" then Bot (read hv) else Output (read hv))
