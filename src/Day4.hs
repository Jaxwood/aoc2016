module Day4 (resulta, resultb) where
  
  import Data.List
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.String

  type Name = String
  type Sector = Int
  type Checksum = String
  data Room = Room Name Sector Checksum deriving (Eq, Show)

  resulta :: String -> Int
  resulta = sumRooms . map (parse match "") . lines

  resultb :: String -> String -> Int
  resultb input code =
    head . map (\(Room _ c _) -> c) . filter (\(Room a b c) -> a == code) . map decipher . rights . map (parse matchb "") . lines $ input

  sumRooms :: [Either ParseError Room] -> Int
  sumRooms = foldl (\acc (Room a b c) -> b + acc) 0 . filter isValid . rights

  isValid :: Room -> Bool
  isValid r@(Room _ _ c) = 
    let a = calculate r
    in a == c

  calculate :: Room -> String
  calculate (Room n _ _) = take 5 . map (head . fst) . sortBy sortGT . map (\x -> (x, length x)) . group . sort $ n

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

  matchb :: Parser Room
  matchb = do
    name <- manyTill anyToken (lookAhead digit)
    sector <- many digit
    _ <- char '['
    checksum <- many1 letter
    return $ Room (init name) (read sector) checksum

  decipher :: Room -> Room
  decipher (Room cipher code checksum) =
    Room (map (decrypt (code+1)) cipher) code checksum

  decrypt :: Int -> Char -> Char
  decrypt i c = head . reverse . take i $ (iterate next c)

  next :: Char -> Char
  next ' ' = ' '
  next '-' = ' '
  next 'z' = 'a'
  next c = succ c 
