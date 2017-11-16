module Day4 (resulta, resultb, parseName, parseSector, parseChecksum) where
  
  import qualified Text.Parsec as Parsec

  resulta :: String -> Int
  resulta str = 0

  resultb :: String -> Int
  resultb str = 0

  parseName :: String -> Either Parsec.ParseError String
  parseName = Parsec.parse (Parsec.endBy Parsec.letter (Parsec.char '-')) ""

  parseSector :: String -> Either Parsec.ParseError String
  parseSector = Parsec.parse (Parsec.many Parsec.digit) ""

  parseChecksum :: String -> Either Parsec.ParseError String
  parseChecksum = do
    _ <- Parsec.parse (Parsec.char '[') ""
    checksum <- Parsec.parse (Parsec.many1 Parsec.letter) ""
    return checksum
