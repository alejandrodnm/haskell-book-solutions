module Ch20.LibraryFunctions where

import           Data.Monoid (Product (..), Sum (..))

{-# ANN fold' "HLint: ignore" #-}

sum' :: (Foldable t, Num a) => t a -> a
sum' ts = getSum $ foldMap Sum ts

product' :: (Foldable t, Num a) => t a -> a
product' ts = getProduct $ foldMap Product ts

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr f False
  where
    f a acc = (a == x) || acc

minimun' :: (Foldable t, Ord a) => t a -> Maybe a
minimun' = foldr f Nothing
  where
    f x Nothing = Just x
    f x (Just y)
        | x < y = Just x
        | otherwise = Just y

-- I read this one in the source code #SHAME
null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length' :: Foldable t => t a -> Int
length' = foldr (\_ b -> b + 1) 0

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\ a b -> f a <> b) mempty
