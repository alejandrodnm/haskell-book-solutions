{-# LANGUAGE FlexibleInstances #-}

module Ch16.Functor where

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

data Three' a b =
    Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c ) = Three' a (f b) (f c)

newtype K a b =
    K a
    deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K a) = K a

newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip $ K (f a)

newtype LiftItOut f a =
    LiftItOut (f a)
    deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut $ f <$> fa

data Parappa f g a = DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

data List a
    = Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (f <$> xs)

data GoatLord a
    = NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a)
                (GoatLord a)
                (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat            = NoGoat
    fmap f (OneGoat a)       = OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (f <$> a) (f <$> b) (f <$> c)

data TalkToMe a
    = Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g)    = Read (f . g)
