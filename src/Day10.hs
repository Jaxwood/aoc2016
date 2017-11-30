module Day10 (resulta, resultb) where

  import Data.List
  import Data.Either (rights)
  import Data.Maybe (fromJust)
  import Text.Parsec (parse, digit, choice, string, space, many1)
  import Text.Parsec.String

  type Value = Int
  type Low = Int
  type High = Int
  data Reciever = Bot Int | Output Int deriving (Show)
  data State = State (Reciever, [Int]) deriving (Show)
  data Instruction = Assign Reciever Value | Logic Int Reciever Reciever deriving (Show)

  resulta :: String -> [Int] -> [State]
  resulta x xs = runInstructions $ parseInstructions $ lines x

  resultb :: String -> String
  resultb = id

  runInstructions :: [Instruction] -> [State]
  runInstructions is = executeInstruction is (foldl state [] is)

  executeInstruction :: [Instruction] -> [State] -> [State]
  executeInstruction is ss = let s = (hasTwo ss)
                               in case s of
                                 Nothing -> ss
                                 Just s@(State (r, vs)) -> let ss' = updateState i ss
                                                           in executeInstruction is ss'
                                                           where i = findInstruction s is
  updateState :: Maybe Instruction -> [State] -> [State]
  updateState (Nothing) ss = ss
  updateState (Just (Logic _ a b)) ss = ss
  updateState (Just (Assign _ _)) ss = ss

  hasTwo :: [State] -> Maybe State
  hasTwo = find (\(State (_,s)) -> length s == 2)

  findInstruction :: State -> [Instruction] -> Maybe Instruction
  findInstruction (State (Bot n,_)) is = find (finder n) is

  finder :: Int -> Instruction -> Bool
  finder no (Logic n _ _) = no == n
  finder no (Assign _ _) = False

  state :: [State] -> Instruction -> [State]
  state ss (Assign (Bot no) val) = let a = find (\(State ((Bot n),_)) -> n == no) ss
                                   in case a
                                     of (Nothing) -> (State ((Bot no), [val])):ss
                                        (Just (State ((Bot no'), val'))) -> (State (Bot no, val:val')):ss'
                                     where ss' = filter (\(State ((Bot no'),val)) -> no /= no') ss
  state ss (Assign (Output no) val) = let a = find (\(State ((Output n),_)) -> n == no) ss
                                      in case a
                                        of (Nothing) -> (State ((Output no), [val])):ss
                                           (Just (State ((Output no'), val'))) -> (State (Output no, val:val')):ss'
                                        where ss' = filter (\(State ((Output no'),val)) -> no /= no') ss
  state ss _ = ss

  parseInstructions :: [String] -> [Instruction]
  parseInstructions = rights . map (parse (choice [parseRecieverInstruction, parseValueInstruction]) "")

  parseRecieverInstruction :: Parser Instruction
  parseRecieverInstruction = do
    _ <- string "bot "
    bot <- many1 digit
    _ <- string " gives low to "
    lowTo <- choice [string "bot", string "output"]
    _ <- space
    low <- many1 digit
    _ <- string " and high to "
    highTo <- choice [string "bot", string "output"]
    _ <- space
    high <- many1 digit
    return $ Logic (read bot) (if lowTo == "bot" then Bot (read low) else Output (read low)) (if highTo == "bot" then Bot (read high) else Output (read high))

  parseValueInstruction :: Parser Instruction
  parseValueInstruction = do
    _ <- string "value "
    v <- many1 digit
    _ <- string " goes to bot "
    b <- many1 digit
    return $ Assign (Bot $ read b) (read v)
