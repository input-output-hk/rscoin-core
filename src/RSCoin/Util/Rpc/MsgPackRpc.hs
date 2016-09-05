{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | msgpack-rpc based implementation of MonadRpc which uses real network.

module RSCoin.Util.Rpc.MsgPackRpc
       ( MsgPackRpc
       , runMsgPackRpc
       ) where

import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader        (ReaderT (..), runReaderT)
import           Control.Monad.Trans         (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl, StM,
                                              liftBaseWith, restoreM)

import           Data.IORef                  (newIORef, readIORef, writeIORef)
import           Data.Maybe                  (fromMaybe)

import qualified Network.MessagePack.Client  as C
import qualified Network.MessagePack.Server  as S

import           RSCoin.Util.Rpc.MonadRpc    (Client (..), Method (..),
                                              MonadRpc (..))
import           RSCoin.Util.Timed           (MonadTimed, TimedIO)

newtype MsgPackRpc a = MsgPackRpc
    { runMsgPackRpc :: TimedIO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow,
                MonadCatch, MonadMask, MonadTimed)

instance MonadBaseControl IO MsgPackRpc where
    type StM MsgPackRpc a = a

    liftBaseWith f = MsgPackRpc $ liftBaseWith $ \g -> f $ g . runMsgPackRpc

    restoreM = MsgPackRpc . restoreM

instance MonadRpc MsgPackRpc where
    execClient (addr, port) (Client name args) = liftIO $ do
        box <- newIORef Nothing
        C.execClient addr port $ do
            -- note, underlying rpc accepts a single argument - [Object]
            res <- C.call name args
            liftIO . writeIORef box $ Just res
        fromMaybe (error "Aaa, execClient didn't return a value!")
            <$> readIORef box

    serve port methods = S.serve port $ convertMethod <$> methods
      where
        convertMethod :: Method MsgPackRpc -> S.Method MsgPackRpc
        convertMethod Method{..} = S.method methodName methodBody

instance MonadRpc m => MonadRpc (ReaderT r m) where
    execClient addr cli = lift $ execClient addr cli

    serve port methods = ReaderT $
                            \r ->  serve port (convert r <$> methods)
      where
        convert :: Monad m => r -> Method (ReaderT r m) -> Method m
        convert r Method {..} =
            Method methodName (flip runReaderT r . methodBody)

instance S.MethodType MsgPackRpc f => S.MethodType MsgPackRpc (MsgPackRpc f)
   where
    toBody res args = res >>= \r -> S.toBody r args
