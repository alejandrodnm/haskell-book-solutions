module Ch26.StateT where

import           Data.Tuple

newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT ma) = StateT $ \ s -> swap <$> (fmap . fmap) f  (swap <$> ma s)

instance (Monad m) => Applicative (StateT s m) where
    pure x = StateT $ \ s -> pure (x, s)

    (StateT mf) <*> sma = StateT $ \ s ->
        do
            (f, s') <- mf s
            runStateT (fmap f sma) s'

instance (Monad m) => Monad (StateT s m) where
    return = pure

    StateT sma >>= f =
        StateT $ \s -> do
            (a, s') <- sma s
            runStateT (f a) s'
