module MyCipher where

import Data.Char

caesarChar :: Char -> Int -> Char
caesarChar c n
  | isAsciiLower c = chr (ord 'a' + (ord c - ord 'a' + n) `mod` 26)
  | isAsciiUpper c = chr (ord 'A' + (ord c - ord 'A' + n) `mod` 26)

unCaesar :: Char -> Int -> Char
-- chr (ord 'a' + (ord c - ord 'a' - n) `mod` 26)
unCaesar c n
  | isAsciiLower c = chr (ord 'a' + (ord c - ord 'a' - n) `mod` 26)
  | isAsciiUpper c = chr (ord 'A' + (ord c - ord 'A' - n) `mod` 26)

offset :: Char -> Int
offset c = (ord c) - (ord 'A')

encrypt :: String -> String -> String
encrypt msg key = go msg 0 []
  where
    go [] _ res = res
    go (x : xs) n res = go xs (n + 1) (caesarChar x (offset (key !! (n `mod` length key))):res)