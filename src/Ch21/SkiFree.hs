module Ch21.SkiFree where

import           Control.Applicative
import           Data.Foldable

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
    fmap f (S nx x) = S (f <$> nx) (f x)

instance (Foldable n, Functor n) => Foldable (S n) where
    foldMap f (S nx x) = foldMap f nx <> f x

instance Traversable n => Traversable (S n) where
    traverse f (S nx x) = S <$> traverse f nx <*> f x
    sequenceA (S nx x) = S <$> sequenceA nx <*> x
