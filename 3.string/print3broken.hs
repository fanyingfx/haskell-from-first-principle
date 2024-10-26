module Print3Broken where
printSecond = do
    putStrLn greeting

main = do
    putStrLn greeting
    printSecond
    where greeting = "Yarrrrr"
