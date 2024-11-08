module MyList where

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

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
