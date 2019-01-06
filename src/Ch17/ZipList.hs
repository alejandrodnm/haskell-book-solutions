module Ch17.ZipList where

import           Ch17.List (List (..))

take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' i (Cons a as) = Cons a (take' (i-1) as)

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ f <$> xs

instance Applicative ZipList' where
    pure a = ZipList' (repeat' a)
    (<*>) (ZipList' Nil) _            = ZipList' Nil
    (<*>) _ (ZipList' Nil)            = ZipList' Nil
    (<*>) (ZipList' fs) (ZipList' as) =  ZipList' $ zip' fs as

zip' :: List (a -> b) -> List a -> List b
zip' Nil _                   = Nil
zip' _ Nil                   = Nil
zip' (Cons f fs) (Cons a as) = Cons (f a) (zip' fs as)

repeat' :: a -> List a
repeat' a =
    xs
  where
    xs = Cons a xs
