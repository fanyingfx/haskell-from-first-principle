module MySTD where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = f x || myAny f xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem x (y : ys) = x == y || myElem x ys

myElem' x = myAny (== x)

myReverse :: [a] -> [a]
myReverse xs = go xs []
  where
    go [] res = res
    go (x : xs) res = go xs (x : res)

squish :: [[a]] -> [a]
squish xxs = go xxs []
  where
    go [] res = res
    go (xs : xxs) res = go xxs (res ++ xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = go f xs []
  where
    go f [] res = res
    go f (x : xs) res = go f xs (res ++ f x)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMaximumBy f [] =
myMaximumBy f xs = go f xs (head xs)
  where
    go f [] maximum = maximum
    go f (x : xs) maximum = case f x maximum of
      GT -> go f xs x
      _ -> go f xs maximum
myMinmumBy f = myMaximumBy (flip f)