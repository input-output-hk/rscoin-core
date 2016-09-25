{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeFamilies          #-}

-- | WorkMode type class.

module RSCoin.Core.WorkMode
       ( WorkMode
       , RealMode
       , EmulationMode

       , runRealModeBank
       , runRealModeUntrusted
       , runRealModeWithContext
       , runEmulationMode
       ) where

import           Control.Monad            (join)
import           Control.Monad.Catch      (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader     (ReaderT, ask, runReaderT)
import           Control.Monad.Trans      (MonadIO (liftIO))
import           System.Random            (StdGen, getStdGen)

import           Control.TimeWarp.Logging (WithNamedLogger (..), setLoggerName)
import           Control.TimeWarp.Rpc     (DelaysSpecifier (..), MonadRpc, MsgPackRpc,
                                           PureRpc, runMsgPackRpc, runPureRpc)
import           Control.TimeWarp.Timed   (MonadTimed (..), runTimedIO, ThreadId)

import           RSCoin.Core.Crypto       (SecretKey)
import           RSCoin.Core.Logging      (LoggerName, bankLoggerName,
                                           nakedLoggerName)
import           RSCoin.Core.NodeConfig   (ContextArgument, NodeContext,
                                           WithNodeContext (..),
                                           defaultNodeContext, mkNodeContext)

class ( MonadTimed m
      , MonadRpc m
      , MonadIO m
      , WithNamedLogger m
      , WithNodeContext m
      , MonadMask m
      ) =>
      WorkMode m

instance ( MonadTimed m
         , MonadRpc m
         , MonadIO m
         , WithNamedLogger m
         , WithNodeContext m
         , MonadMask m
         ) =>
         WorkMode m

newtype ContextHolder m a = ContextHolder
    { getContextHolder :: ReaderT NodeContext m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTimed, MonadRpc, WithNamedLogger)

type instance ThreadId (ContextHolder m) = ThreadId m

instance Monad m => WithNodeContext (ContextHolder m) where
    getNodeContext = ContextHolder ask

type RealMode = ContextHolder MsgPackRpc

runRealModeWithContext :: MonadIO m => NodeContext -> RealMode a -> m a
runRealModeWithContext nodeContext =
   liftIO . runTimedIO . runMsgPackRpc . setLoggerName nakedLoggerName
          . flip runReaderT nodeContext . getContextHolder

runRealModeBank :: ContextArgument -> SecretKey -> RealMode a -> IO a
runRealModeBank ca bankSecretKey bankAction = do
    ctx <- mkNodeContext (Just bankSecretKey) ca
    runRealModeWithContext ctx $
        setLoggerName bankLoggerName bankAction

runRealModeUntrusted :: LoggerName -> ContextArgument -> RealMode a -> IO a
runRealModeUntrusted logName ca nodeAction = do
    untrustedNodeContext <- mkNodeContext Nothing ca
    runRealModeWithContext untrustedNodeContext $
        setLoggerName logName nodeAction

type EmulationMode = ContextHolder (PureRpc IO)

runEmulationMode ::
    (MonadIO m, DelaysSpecifier delays) =>
    Maybe StdGen -> delays -> EmulationMode a -> m a
runEmulationMode genMaybe delays action =
    liftIO . join $
    runPureRpc <$> getGen genMaybe <*> pure (toDelays delays) <*> pure action'
  where
    action' = setLoggerName nakedLoggerName
            . flip runReaderT defaultNodeContext
            . getContextHolder
            $ action

getGen :: Maybe StdGen -> IO StdGen
getGen = maybe getStdGen pure
