module Ch18.Exercises where

import           Ch17.List
import           Control.Applicative (liftA2)

{-# ANN j "HLint: ignore" #-}

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    (>>=) _ f = NopeDotJpg

data Either' b a
    = Left' a
    | Right' b
    deriving (Eq, Show)

instance Functor (Either' b) where
    fmap _ (Right' b) = Right' b
    fmap f (Left' a)  = Left' (f a)

instance Applicative (Either' b) where
    pure = Left'

    Right' b <*> _ = Right' b
    _ <*> Right' b = Right' b
    Left' f <*> Left' a = Left' (f a)

instance Monad (Either' b) where
    (>>=) (Right' b) _ = Right' b
    (>>=) (Left' a) f  = f a

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a

instance Monad List where
    (>>=) Nil _         = Nil
    (>>=) (Cons a as) f = f a <> (as >>= f)

j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _       = pure []
meh (x : xs) f = liftA2 mappend ((:[]) <$> f x) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType l = meh l id
