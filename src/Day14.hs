module Day14 (resulta) where

  import qualified Data.ByteString.Char8 as C
  import Crypto.Hash

  md5 :: C.ByteString -> Digest MD5
  md5 = hash

  generateMD5 :: String -> [String]
  generateMD5 s = map (show . md5 . C.pack . (++) s . show) [0..]

  resulta :: String -> Int -> [String]
  resulta salt no = take 20 $ generateMD5 salt
