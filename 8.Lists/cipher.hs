module Cipher where

import Data.Char

encryptChar :: Char -> Int -> Char
encryptChar c n
  | isAsciiLower c = chr (ord 'a' + (ord c - ord 'a' + n) `mod` 26)
  | isAsciiUpper c = chr (ord 'A' + (ord c - ord 'A' + n) `mod` 26)

unCaesar :: Char -> Int -> Char
-- chr (ord 'a' + (ord c - ord 'a' - n) `mod` 26)
unCaesar c n
  | isAsciiLower c = chr (ord 'a' + (ord c - ord 'a' - n) `mod` 26)
  | isAsciiUpper c = chr (ord 'A' + (ord c - ord 'A' - n) `mod` 26)