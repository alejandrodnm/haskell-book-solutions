module Ch23.Exercises where

import           Ch23.State

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

exec :: State s a -> s -> s
exec (State sa) s =
    let (a, s) = sa s
     in s

exec' :: State s a -> s -> s
exec' (State sa) s = snd (sa s)

eval :: State s a -> s -> a
eval (State sa) s = fst (sa s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
