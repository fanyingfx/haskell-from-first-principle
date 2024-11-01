module EqTest where

newtype TisAnInteger = TisAn Integer

data TwoIntegers = Two Integer Integer

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (TisAnInt i) == (TisAnInt i') = i == i'
  (TisAString s) == (TisAString s') = s == s'
  (==) _ _ = False

data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne b) (ThatOne b') = b == b'
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == a' && b == b'
