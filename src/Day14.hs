module Day14 (resulta) where

  import qualified Data.ByteString.Char8 as C
  import Crypto.Hash
  import Data.Maybe

  md5 :: C.ByteString -> Digest MD5
  md5 = hash

  transform :: (Int -> String) -> Int -> (Int, Maybe Char, Maybe Char)
  transform fn n = (n, threes candidate, fives candidate)
    where candidate = fn n

  generateMD5 :: String -> [(Int, Maybe Char, Maybe Char)]
  generateMD5 s = map (transform fn) [0..]
    where fn = (show . md5 . C.pack . (++) s . show)

  threes :: String -> Maybe Char
  threes (a:b:[]) = Nothing
  threes (a:b:c:xs)
    | a == b && a == c = Just a
    | otherwise = threes (b:c:xs)

  fives :: String -> Maybe Char
  fives (a:b:c:d:[]) = Nothing
  fives (a:b:c:d:e:xs)
    | all (==a) [b,c,d,e] = Just a
    | otherwise = fives (b:c:d:e:xs)

  five :: Maybe Char -> (Int, Maybe Char, Maybe Char) -> Bool
  five c (_, _, f) = f == c

  hasAFive :: [(Int, Maybe Char, Maybe Char)] -> (Int, Maybe Char, Maybe Char) -> Bool
  hasAFive stream (_, Nothing, _) = False
  hasAFive stream (idx, c, _) = any (five c) $
    takeWhile ((<= (idx + 1000)) . tpt) $
    dropWhile ((<= idx) . tpt) stream

  -- extract a triplet
  tpt :: (a, b, c) -> a
  tpt (a, _, c) = a

  resulta :: String -> Int -> Int
  resulta salt no = tpt $ last $ take no $ filter (hasAFive md5stream) md5stream
    where md5stream = generateMD5 salt
