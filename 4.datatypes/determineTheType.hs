module DetermineTheType where

example = 1

x1 = head [(0, "doage")]

x = 5

y = x + 5

f = 4 / y

myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc f g _ (a, x) = (a, g $ f x)

-- 2 .
c :: a -> b -> a
c a b = a

-- 3
c'' :: b -> a -> b
c'' = c

-- 4
c' :: a -> b -> b
c' a b = b

-- 5
r :: [a] -> [a]
-- r [a] = [a]
r (x : xs) = xs

-- 6

co :: (b -> c) -> (a -> b) -> a -> c
co f g a = f $ g  a

-- 7
a :: (a->c) -> a ->a
a f a = a

-- 8
a' :: (a->b)->a->b
a' f = f 
