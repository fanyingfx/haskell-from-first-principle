module MyTypeClass where

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age = Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber = Age
  toNumber (Age a) = a
  defaultNumber = Age 65

newtype Year = Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber = Year
  toNumber (Year y) = y
  defaultNumber = Year 1998

sumNumberish :: (Numberish a) => a -> a -> a
sumNumberish a a' = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfAPrime = toNumber a'
    summed = integerOfA + integerOfAPrime

addWeird :: (Ord a,Num a) => a->a->a
addWeird x y =
    if x > 1
    then x + y
    else x