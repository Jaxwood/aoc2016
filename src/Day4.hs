module Day4 (resulta, resultb) where
  
  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  type Name = String
  type Sector = Int
  type Checksum = String
  data Room = Room Name Sector Checksum deriving (Eq, Show)

  resulta :: String -> Int
  resulta str = 0 -- foldl (\(Room _ s _) a -> s+a) 0 . filter (\(Room a _ c) -> a == c) rcalculate . parseInput

  resultb :: String -> Int
  resultb str = 0

  calculate :: Room -> Room
  calculate (Room n s c) =
    let a = (Room a s c) in
      take 5 . map head . map fst . sortBy sortGT . map (\x -> (x, length x)) . group . sort $ n

  parseInputResult :: String -> Either ParseError Room
  parseInputResult = parse parseInput ""

  sortGT :: (String, Int) -> (String, Int) -> Ordering
  sortGT (a1, b1) (a2, b2)
    | b1 < b2 = GT
    | b1 > b2 = LT
    | b1 == b2 = compare a1 a2

  parseInput :: Parser Room
  parseInput = do
    name <- endBy (many letter) (char '-')
    sector <- many digit
    _ <- char '['
    checksum <- many1 letter
    return $ Room (concat name) (read sector) checksum
