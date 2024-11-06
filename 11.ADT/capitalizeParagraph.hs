module MyCapitalize where

import Data.Char

capitalizeWord :: String -> String
capitalizeWord (x : xs) = toUpper x : xs

-- capitalizeWord "Titter"
-- capitalizeWord "titter"
capitalizeParagraph :: String -> String
capitalizeParagraph para = unwords (capitalizeSentence (words para))
  where
    isEndWord [] = False
    isEndWord word = (last word) == '.'
    capitalizeSentence [] = []
    capitalizeSentence xxs@(x : xs) = capitalizeWord x : capitalizeSentenceRest xxs
    -- capitalizeFirst [] = []
    -- capitalizeFirst (x : xs) = capitalizeWord x : xs
    -- capitalizeSentenceRest [] = []
    capitalizeSentenceRest (prev : cur : rest) =
      if isEndWord prev
        then capitalizeWord cur : capitalizeSentenceRest (cur:rest)
        else cur : capitalizeSentenceRest (cur : rest)
    capitalizeSentenceRest _ = []

-- capitalizeSentence [xs] = [xs]

-- capitalizeParagraph "blan. woot ha."