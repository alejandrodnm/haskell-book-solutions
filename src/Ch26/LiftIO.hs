module Ch26.LiftIO where

import           Ch26.LiftMore
import           Ch26.StateT
-- import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO
