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

# IO

As we’ve seen in the previous chapters, GHC can normally reorder operations. This is disabled in IO (as in ST). IO actions are instead enclosed within nested lambdas — nesting is the only way to ensure that actions are sequenced within a pure lambda calculus.

The underlying representation of IO allows the actions to be nested, and therefore sequenced.


In addition to enforcing ordering, IO turns off a lot of the sharing we talked about in the nonstrictness chapter.

## Sharing
Values of type `IO a` are not an `a`;they’re adescription of how you might get an `a`.
