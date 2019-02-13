-- instance types provided as
-- they may help.
{-# LANGUAGE InstanceSigs #-}

module Ch25.Compose where

import           Data.Foldable

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose (pure . pure $ a)

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose l) = (foldMap . foldMap) f l

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    -- (a -> f b) -> t a -> f (t b)
    traverse f (Compose l) = Compose <$> (traverse . traverse) f l
