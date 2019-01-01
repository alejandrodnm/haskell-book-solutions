{-# LANGUAGE TupleSections #-}

module Ch15.Monoid where

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

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

instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine mempty

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    Success a <> _ = Success a
    _ <> Success a = Success a
    Failure a <> Failure a' = Failure (a <> a')

newtype Mem s a = Mem { runMem :: s -> (a, s) }

combineMem
    :: Semigroup a
    => (s -> (a, s))
    -> (s -> (a, s))
    -> s
    -> (a, s)
combineMem f g x =
  let
    (a, s) = f x
    (a', s') = g s
  in
    (a <> a', s')

instance Semigroup a => Semigroup (Mem s a) where
    Mem { runMem = f } <> Mem { runMem = g } = Mem (combineMem f g)

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (mempty,)
