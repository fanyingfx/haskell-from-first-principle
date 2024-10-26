letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

s = "Curry is awesome"
rvrs :: String-> String
rvrs x =take 7 (drop 9 x) ++ " " ++ take 2 (drop 6 x) ++ " " ++ take 5 x

main :: IO()
main = print (rvrs s)