module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> Char -> [String]
myLines lines delimter = go lines []
  where
    go lines res
      | lines == "" = res
      | otherwise = go (dropWhile (== delimter) (dropWhile (/= delimter) lines)) (res ++ [takeWhile (/= delimter) lines])

shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences '\n' == shouldEqual)