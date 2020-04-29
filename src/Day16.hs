module Day16 (resulta) where

  inverse :: Char -> Char
  inverse c
    | c == '0' = '1'
    | c == '1' = '0'

  checksum :: String -> String -> String
  checksum acc [] = if even (length acc) then checksum "" acc else acc
  checksum acc (a:b:xs)
    | a == b = checksum ('1':acc) xs
    | otherwise = checksum ('0':acc) xs

  resulta :: Int -> String -> String
  resulta len a = if length a' < len then resulta len a' else checksum "" $ take len a'
    where b = map inverse $ reverse a
          a'= a ++ "0" ++ b
