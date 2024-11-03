module MyChar where

import Data.Char

filterUppercase :: String -> String
filterUppercase = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

upperString :: String -> String
upperString = map toUpper
-- upperString [] = []
-- upperString (x:xs) = toUpper x : upperString xs
capitalizeHead :: String -> Char
-- capitalizeHead [] = ' '
capitalizeHead = toUpper .  head