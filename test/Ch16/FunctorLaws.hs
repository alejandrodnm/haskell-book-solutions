module Ch16.FunctorLaws where

import           Test.QuickCheck

{-# ANN functorCompose "HLint: ignore" #-}
{-# ANN functorCompose' "HLint: ignore" #-}
{-# ANN functorIdentity "HLint: ignore" #-}

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x =
        (fmap (g . f) x) == (fmap g . fmap f $ x)
