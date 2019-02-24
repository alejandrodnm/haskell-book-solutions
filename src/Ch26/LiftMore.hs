module Ch26.LiftMore where

import           Ch26.EitherT
import           Ch26.StateT
import           Control.Monad.Trans.Class

instance MonadTrans (EitherT e) where
    lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> do
        a <- ma
        return (a, s)
