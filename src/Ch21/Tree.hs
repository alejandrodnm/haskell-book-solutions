module Ch21.Tree where

data Tree a
    = Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap f Empty          = Empty
    fmap f (Leaf x)       = Leaf (f x)
    fmap f (Node t1 x t2) = Node (fmap f t1) (f x) (fmap f t2)

-- foldMap is a bit easier and looks more natural, but you can do foldr
-- too for extra credit.
instance Foldable Tree where
    foldMap _ Empty          = mempty
    foldMap f (Leaf x)       = f x
    foldMap f (Node t1 x t2) = foldMap f t1 <> f x <> foldMap f t2

    foldr _ acc Empty          = acc
    foldr f acc (Leaf x)       = f x acc
    foldr f acc (Node t1 x t2) = f x $ foldr f (foldr f acc t2) t1

instance Traversable Tree where
    traverse _ Empty          = pure Empty
    traverse f (Leaf x)       = Leaf <$> f x
    traverse f (Node t1 x t2) = Node <$> traverse f t2 <*> f x <*> traverse f t2
