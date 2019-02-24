module Ch26.EitherT where

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT (pure (pure x))

    EitherT fab <*> EitherT mea = EitherT $ (<*>) <$> fab <*> mea

instance Monad m => Monad (EitherT e m) where
    return = pure

    (EitherT ma) >>= f =
        EitherT $ do
            ea <- ma
            case ea of
              Left e  -> return (Left e)
              Right a -> runEitherT (f a)

swapEither :: Either a b -> Either b a
swapEither (Left b)  = Right b
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) =
    EitherT $ swapEither <$> mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT mab) = do
    eab <- mab
    case eab of
      Left a  -> fa a
      Right b -> fb b
