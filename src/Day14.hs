module Day14 (resulta) where

  import qualified Data.ByteString.Char8 as C
  import Crypto.Hash

  md5 :: C.ByteString -> Digest MD5
  md5 = hash

  generateMD5 :: String -> [(Int, String)]
  generateMD5 s = map (\n -> (n, fn n)) [0..]
    where fn = (show . md5 . C.pack . (++) s . show)

  repeats :: (Int, String) -> Bool
  repeats (_, (a:b:[])) = False
  repeats (n, (a:b:c:xs))
    | a == b && a == c = True
    | otherwise = repeats (n, (b:c:xs))

  resulta :: String -> Int -> [(Int, String)]
  resulta salt no = take 5 $ filter repeats $ generateMD5 salt
