module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
-- digitToWord n = undefined
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = digits (n `div` 10) ++ digits (n `mod` 10)

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord . digits $ n)