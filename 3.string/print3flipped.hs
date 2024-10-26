module Print3Flipped where
myGreeting = (++) "hello" "world"

hello = "world"
world = "world"

main :: IO()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting=
            (++) hello ((++) " " world)