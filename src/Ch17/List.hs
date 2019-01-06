module Ch17.List where

data List a
    = Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Semigroup (List a) where
    (<>) a Nil          = a
    (<>) Nil a          = a
    (<>) (Cons a xa) xb = Cons a (xa <> xb)

instance Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative List where
    pure a = Cons a Nil

    (<*>) _ Nil         = Nil
    (<*>) Nil _         = Nil
    (<*>) (Cons f fs) x = (f <$> x) <> (fs <*> x)
