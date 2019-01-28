{-# LANGUAGE InstanceSigs #-}
module Ch21.Exercises where

import           Ch17.Constant
import           Ch17.Identity

instance Foldable Identity where
    foldMap :: Monoid m => (a -> m) -> Identity a -> m
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse :: (Functor f) => (a -> f b) -> Identity a -> f (Identity b)
    traverse f (Identity x) = Identity <$> f x
    sequenceA (Identity x) =  Identity <$> x

instance Foldable (Constant a) where
    foldMap :: Monoid m => (b -> m) -> Constant a b -> m
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    traverse :: (Applicative f) => (b -> f c) -> Constant a b -> f (Constant a c)
    traverse _ (Constant a) = pure (Constant a)
    sequenceA (Constant a) =  pure (Constant a)

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
    foldMap _ Nada    = mempty
    foldMap f (Yep a) = f a

instance Functor Optional where
    fmap _ Nada    = Nada
    fmap f (Yep a) = Yep $ f a

instance Traversable Optional where
    traverse _ Nada    = pure  Nada
    traverse f (Yep a) = Yep <$> f a

    sequenceA Nada    = pure Nada
    sequenceA (Yep a) = Yep <$> a

data List a = Nil
            | Cons a (List a)

instance Semigroup (List a) where
    Nil <> Nil        = Nil
    x <> Nil          = x
    Nil <> x          = x
    (Cons x xs) <> ys = Cons x (xs <> ys)

instance Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Foldable List where
    foldMap _ Nil         = mempty
    foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
    traverse _ Nil         = pure Nil
    traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

    sequenceA Nil         = pure Nil
    sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c
    sequenceA (Three a b c) = Three a b <$> c

data Big a b = Big a b b

instance Functor (Big a) where
    fmap f (Big a x y) = Big a (f x) (f y)

instance Foldable (Big a) where
    foldMap f (Big a x y) = f x <> f y

instance Traversable (Big a) where
    traverse f (Big a x y) = Big a <$> f x <*> f y
