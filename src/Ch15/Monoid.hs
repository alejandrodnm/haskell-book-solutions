module Ch15.Monoid where

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity (a <> b)

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two a' b' = Two (a <> a') (b <> b')

data Or a b = Fst a | Snd b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    Snd a <> _ = Snd a
    Fst a <> Snd a' = Snd a'
    Fst a <> Fst a' = Fst a'

newtype Combine a b =
    Combine { unCombine :: a -> b }

instance (Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (f <> g)

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    Success a <> _ = Success a
    _ <> Success a = Success a
    Failure a <> Failure a' = Failure (a <> a')
