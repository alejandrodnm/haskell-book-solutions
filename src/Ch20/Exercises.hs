module Ch20.Exercises where

newtype Constant a b = Constant b

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

data Two a b = Two a b

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' _ b c) = f b <> f c

filterF :: (Applicative f , Foldable t , Monoid (f a))
        => (a -> Bool)
        -> t a
        -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
