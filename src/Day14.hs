module Day14 (resulta,resultb) where

  import qualified Data.ByteString.Char8 as C
  import Crypto.Hash
  import Data.Maybe

  -- alias hash to md5
  md5 :: C.ByteString -> Digest MD5
  md5 = hash

  -- transform the stream
  transform :: (Int -> String) -> Int -> (Int, [Maybe Char])
  transform fn n = (n, [threes candidate, fives candidate])
    where candidate = fn n

  -- generate an md5 stream
  generateMD5 :: String -> [(Int, [Maybe Char])]
  generateMD5 s = map (transform fn) [0..]
    where fn = (show . md5 . C.pack . (++) s . show)

  -- securely transform the stream
  secureTransform :: (String -> String) -> String -> Int -> (Int, [Maybe Char])
  secureTransform fn s n = (n, [threes candidate, fives candidate])
    where candidate = foldr (\_ acc -> fn acc) start [0..2016]
          start = s ++ (show n)

  -- generate a secure md5 stream
  secureMD5 :: String -> [(Int, [Maybe Char])]
  secureMD5 s = map (secureTransform fn s) [0..]
    where fn = (show . md5 . C.pack)

  -- filter for three consecutive characters
  threes :: String -> Maybe Char
  threes (a:b:[]) = Nothing
  threes (a:b:c:xs)
    | a == b && a == c = Just a
    | otherwise = threes (b:c:xs)

  -- filter for fives consecutive characters
  fives :: String -> Maybe Char
  fives (a:b:c:d:[]) = Nothing
  fives (a:b:c:d:e:xs)
    | all (==a) [b,c,d,e] = Just a
    | otherwise = fives (b:c:d:e:xs)

  -- check if it is a valid key
  isValid :: [(Int, [Maybe Char])] -> (Int, [Maybe Char]) -> Bool
  isValid stream (_, [Nothing, _]) = False
  isValid stream (idx, [c, _]) = any ((== c) . last . snd) $
    takeWhile ((<= (idx + 1000)) . fst) $
    dropWhile ((<= idx) . fst) stream

  resulta :: String -> Int -> Int
  resulta salt no = fst $ last $ take no $ filter (isValid md5stream) md5stream
    where md5stream = generateMD5 salt

  resultb :: String -> Int -> Int
  resultb salt no = fst $ last $ take no $ filter (isValid md5stream) md5stream
    where md5stream = secureMD5 salt
