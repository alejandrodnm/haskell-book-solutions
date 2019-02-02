module Ch22.ReaderPractice where

import           Control.Applicative
import           Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' x []          = Nothing
lookup' x ((a, b):xs) = if x == a then Just b else lookup x xs

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup' 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup' 6 $ zip y z

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup' 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup' n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = let z = z' n in (z, z)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main' :: IO ()
main' = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print "summed <$> ((,) <$> xs <*> ys)"
    print $ summed <$> ((,) <$> xs <*> ys)
    print "fmap summed ((,) <$> xs <*> zs)"
    print $ fmap summed ((,) <$> xs <*> zs)
    print "bolt 7"
    print $ bolt 7
    print "fmap bolt z"
    print $ fmap bolt z
    print "sequenceA [(>3), (<8), even] 7"
    print $ sequenceA [(>3), (<8), even] 7

main'' :: IO()
main'' = do
    print "foldr (&&) True (sequA 4)"
    print $ foldr (&&) True (sequA 4)
    print "(fromMaybe 42 s')"
    print $ sequA (fromMaybe 42 s')
    print $ bolt (fromMaybe 24 ys)
