module Ch26.Exercises where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = reader $ flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
    liftIO $ putStrLn ("HI: " ++ show r)
    return $ r + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \r -> do
    liftIO $ putStrLn ("HI: " ++ show r)
    return (show r, r + 1)
