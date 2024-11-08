module MyZipList where
import Test.QuickCheck.Checkers

data List a = Nil | Cons a (List a) deriving (Eq, Show)
take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)


append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

fold :: (a->b->b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List(List a) -> List a
concat' = fold append Nil
flatMap :: (a->List b)-> List a -> List b
flatMap f xs = concat' (fmap f xs)

instance (Semigroup a) => Semigroup (List a) where
  Nil <> ys = ys
  ys <> Nil = ys
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance (Monoid a) => Monoid (List a) where
  mempty = Nil

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f xsf) xs = fmap f xs `append` (xsf <*> xs)

-- zipApply :: List (a->b)->List a -> List b

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)
instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                in take' 3000 l
              ys' = let (ZipList' l) = ys
                in take' 3000 l
instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs
instance Applicative ZipList' where
    pure a= ZipList' (Cons a Nil)
    (<*>) (ZipList' fl) (ZipList' al) =ZipList' (zipApply fl al) 
        where
            zipApply _ Nil = Nil
            zipApply Nil _ = Nil
            zipApply (Cons f xsf) (Cons x xs) = Cons (f x) (zipApply xsf xs)