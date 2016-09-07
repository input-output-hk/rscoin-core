{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | WorkMode type class.

module RSCoin.Core.WorkMode
       ( WorkMode
       , RealMode
       , EmulationMode

       , runRealModeBank
       , runRealModeUntrusted
       , runEmulationMode
       , runEmulationMode_
       ) where

import           Control.Lens             (view, (%~), iso)
import           Control.Monad            (join)
import           Control.Monad.Catch      (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader     (ReaderT, ask, runReaderT, local)
import           Control.Monad.Trans      (MonadIO (liftIO))
import           System.Random            (StdGen, getStdGen)

import           Control.TimeWarp.Logging (WithNamedLogger (..))
import           Control.TimeWarp.Rpc     (Delays, MonadRpc, MsgPackRpc,
                                           PureRpc, runMsgPackRpc, runPureRpc,
                                           runPureRpc_)
import           Control.TimeWarp.Timed   (MonadTimed, runTimedIO)

import           RSCoin.Core.Crypto       (SecretKey)
import           RSCoin.Core.Logging      (LoggerName, bankLoggerName)
import           RSCoin.Core.NodeConfig   (ContextArgument, NodeContext,
                                           WithNodeContext (..), ctxLoggerName,
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
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTimed, MonadRpc)

instance Monad m => WithNamedLogger (ContextHolder m) where
    getLoggerName = ContextHolder $ view ctxLoggerName

    modifyLoggerName how = ctxHolder %~ local (ctxLoggerName %~ how)
      where
        ctxHolder = iso getContextHolder ContextHolder

instance Monad m => WithNodeContext (ContextHolder m) where
    getNodeContext = ContextHolder ask

type RealMode = ContextHolder MsgPackRpc

runRealModeWithContext :: NodeContext -> RealMode a -> IO a
runRealModeWithContext nodeContext =
    runTimedIO . runMsgPackRpc . flip runReaderT nodeContext . getContextHolder

runRealModeBank :: ContextArgument -> SecretKey -> RealMode a -> IO a
runRealModeBank ca bankSecretKey bankAction = do
    ctx <- mkNodeContext bankLoggerName (Just bankSecretKey) ca
    runRealModeWithContext ctx bankAction

runRealModeUntrusted :: LoggerName -> ContextArgument -> RealMode a -> IO a
runRealModeUntrusted logName ca nodeAction = do
    untrustedNodeContext <- mkNodeContext logName Nothing ca
    runRealModeWithContext untrustedNodeContext nodeAction

type EmulationMode = ContextHolder (PureRpc IO)

runEmulationMode :: MonadIO m => Maybe StdGen -> Delays -> EmulationMode a -> m a
runEmulationMode genMaybe delays (flip runReaderT defaultNodeContext .
                                  getContextHolder -> action) =
    liftIO . join $
    runPureRpc <$> getGen genMaybe <*> pure delays <*> pure action

runEmulationMode_ :: MonadIO m => Maybe StdGen -> Delays -> EmulationMode () -> m ()
runEmulationMode_ genMaybe delays (flip runReaderT defaultNodeContext .
                                   getContextHolder -> action) =
    liftIO . join $
    runPureRpc_ <$> getGen genMaybe <*> pure delays <*> pure action

getGen :: Maybe StdGen -> IO StdGen
getGen = maybe getStdGen pure
