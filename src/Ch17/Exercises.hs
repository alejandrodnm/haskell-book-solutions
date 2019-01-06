module Ch17.Exercises where

import           Ch16.Functor


instance Applicative Pair where
    pure a = Pair a a

    Pair f f' <*> Pair a a' = Pair (f a) (f' a')

instance Monoid a => Applicative (Two a) where
    pure = Two mempty

    Two a f <*> Two a' b = Two (a <> a') (f b)

instance Monoid a => Applicative (Three' a) where
    pure b = Three' mempty b b

    Three' a f f' <*> Three' a' b b' = Three' (a <> a') (f b) (f' b')
