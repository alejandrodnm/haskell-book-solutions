{-# LANGUAGE OverloadedStrings #-}

module Ch26.HitCounter where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty.Trans

data Config = Config {
    -- that's one, one click!
    -- two...two clicks!
    -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer),
    prefix :: Text
}

type Scotty =
    ScottyT Text (ReaderT Config IO)

type Handler =
    ActionT Text (ReaderT Config IO)


bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
    case M.lookup k m of
        Just v ->
            let v' = v + 1
             in (M.update (\_ -> Just v') k m, v')
        Nothing -> (M.insert k 1 m, 1)

app :: Scotty ()
app =
    get "/:key" $ do
        unprefixed <- param "key" :: ActionT Text (ReaderT Config IO) Text
        config <- lift ask
        let key' = prefix config <> unprefixed
        newInteger <- liftIO $ do
            m <- readIORef (counts config)
            let (m', v) = bumpBoomp key' m
            modifyIORef (counts config) (const m')
            return v
        html $
            mconcat [ "<h1>Success! Count was: "
                    , TL.pack $ show newInteger
                    , "</h1>"
                    ]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    counts <- newIORef (M.empty :: M.Map Text Integer)
    let
        config = Config
            { counts = counts
            , prefix = TL.pack prefixArg
            }
        runR m = runReaderT m config
    scottyT 3000 runR app
