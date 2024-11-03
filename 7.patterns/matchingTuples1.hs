module TupleFunctions where

addEmUp2 :: (Num a) => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: (Num a) => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

addEmUp2PF :: (Num a) => (a, a) -> a
addEmUp2PF = uncurry (+)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, c) = c
