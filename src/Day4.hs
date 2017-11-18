module Day4 (resulta, resultb, calculate, Room(Room), isValid, sumRooms) where
  
  import Data.List
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.String

  type Name = String
  type Sector = Int
  type Checksum = String
  data Room = Room Name Sector Checksum deriving (Eq, Show)

  resulta :: String -> Int
  resulta = sumRooms . map (parseInput) . lines

  resultb :: String -> Int
  resultb str = 0

  sumRooms :: [Either ParseError Room] -> Int
  sumRooms = foldl (\acc (Room a b c) -> b + acc) 0 . filter isValid . rights

  isValid :: Room -> Bool
  isValid r = 
    let (Room a _ c) = calculate r
    in a == c

  calculate :: Room -> Room
  calculate (Room n s c) =
    let a = take 5 . map head . map fst . sortBy sortGT . map (\x -> (x, length x)) . group . sort $ n
      in (Room a s c)

  parseInput :: String -> Either ParseError Room
  parseInput = parse match ""

  sortGT :: (String, Int) -> (String, Int) -> Ordering
  sortGT (a1, b1) (a2, b2)
    | b1 < b2 = GT
    | b1 > b2 = LT
    | b1 == b2 = compare a1 a2

  match :: Parser Room
  match = do
    name <- endBy (many letter) (char '-')
    sector <- many digit
    _ <- char '['
    checksum <- many1 letter
    return $ Room (concat name) (read sector) checksum
