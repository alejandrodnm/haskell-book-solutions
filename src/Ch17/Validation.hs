module Ch17.Validation where

data Validation e a
    = Failure e
    | Success a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
    pure = Success

    Failure e <*> Success a = Failure e
    Success a <*> Failure e = Failure e
    Success f <*> Success a = Success (f a)
    Failure e <*> Failure e' = Failure (e `mappend` e')
