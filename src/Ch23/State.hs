{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module Ch23.State where

newtype State s a =
    State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State $ \s ->
        let (a, s') = g s
         in (f a, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (a,)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State f) <*> (State g) = State $ \s ->
        let (f', s') = f s
            (a, s'') = g s'
        in
            (f' a, s'')

instance Monad (State s) where
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State f) >>= g = State $ \s ->
        let (a, s') = f s
         in runState (g a) s'
