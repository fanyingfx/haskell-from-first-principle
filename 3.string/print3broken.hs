module Print3Broken where
printSecond = do
    putStrLn greeting
    where greeting = "Yarrrrr"

main = do
    putStrLn greeting
    printSecond
    where greeting = "Yarrrrr"
