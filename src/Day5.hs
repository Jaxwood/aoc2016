module Day5 (resulta, resultb) where

  import qualified Data.ByteString.Lazy as LB
  import qualified Data.ByteString.Builder as LBB
  import Crypto.Hash

  resulta :: String -> String
  resulta = show . md5 . LBB.toLazyByteString . LBB.stringUtf8

  resultb :: String -> Int
  resultb str = 0

  md5 :: LB.ByteString -> Digest MD5
  md5 = hashlazy
