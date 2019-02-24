# Transformers

```haskell
newtype ExceptT e m a =
  ExceptT { runExceptT :: m (Either e a) } 

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) } 
  
newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

class MonadTrans t where
  -- | Lift a computation from
  --   the argument monad to
  -- the constructed monad.
  lift :: (Monad m) => m a -> t m a

class (Monad m) => MonadIO m where 
  -- | Lift a computation
  -- from the 'IO' monad. 
  liftIO :: IO a -> m a
```
