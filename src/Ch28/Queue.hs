module Ch28.Queue where

data Queue a = Queue
    { enqueue :: [a]
    , dequeue :: [a]
    } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x q = case enqueue q of
    [] -> Queue [x] (dequeue q)
    xs -> Queue (x:xs) (dequeue q)

pop :: Queue a -> Maybe (a, Queue a)
pop q = case dequeue q of
    []     -> case enqueue q of
                [] -> Nothing
                xs -> pop (Queue [] (reverse xs))
    (x:xs) -> Just (x, Queue (dequeue q) xs)
