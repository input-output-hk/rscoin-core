{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-- | This module contains MonadRpc which abstracts over RPC communication.

module RSCoin.Util.Rpc.MonadRpc
       ( Port
       , Host
       , NetworkAddress

       , MonadRpc (serve, execClient)
       , RpcType
       , execClientTimeout
       , Method(..)
       , Client(..)
       , method
       , call
       , S.Server
       , S.ServerT
       , S.MethodType
       , C.RpcError(..)
       , serverTypeRestriction0
       , serverTypeRestriction1
       , serverTypeRestriction2
       , serverTypeRestriction3
       , serverTypeRestriction4
       , serverTypeRestriction5
       ) where

import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Trans        (lift)
import           Data.ByteString            (ByteString)

import           Data.MessagePack.Object    (MessagePack, Object (..), toObject)
import           Data.Time.Units            (TimeUnit, convertUnit)

import qualified Network.MessagePack.Client as C
import qualified Network.MessagePack.Server as S

import           RSCoin.Util.Logging        (WithNamedLogger (getLoggerName))
import           RSCoin.Util.Timed          (MonadTimed (timeout))

type Port = Int
type Host = ByteString
type NetworkAddress = (Host, Port)


instance (Monad m, WithNamedLogger m) => WithNamedLogger (S.ServerT m) where
    getLoggerName = lift getLoggerName

-- | Defines protocol of RPC layer
class MonadThrow r => MonadRpc r where
    execClient :: MessagePack a => NetworkAddress -> Client a -> r a
    serve :: Port -> [Method r] -> r ()

-- | Same as MonadRpc, but we can set delays on per call basis.
--   MonadRpc also has specified delays, but only for whole network.
--   Default delay would be thrown away.
--   (another approach: stack this and default delays,
--    makes sense, as in RealRpc we still have default delays, even a little
--    but it can be not convinient in pure implementation)
-- TODO: Do we actually need this?
-- class (MonadRpc r, MonadTimed r) => RpcDelayedMonad r where
--    execClientWithDelay  :: RelativeToNow -> NetworkAddress -> Client a -> r ()
--    serveWithDelay :: RelativeToNow -> Port -> [S.Method r] -> r ()

execClientTimeout
    :: (MonadTimed m, MonadRpc m, MessagePack a, TimeUnit t)
    => t -> NetworkAddress -> Client a -> m a
execClientTimeout (convertUnit -> t) addr = timeout t . execClient addr

-- * Client part

-- | Creates a function call. It accepts function name and arguments
call :: RpcType t => String -> t
call name = rpcc name []

-- | Collects function name and arguments
-- (it's MessagePack implementation is hiden, need our own)
class RpcType t where
    rpcc :: String -> [Object] -> t

instance (RpcType t, MessagePack p) => RpcType (p -> t) where
    rpcc name objs p = rpcc name $ toObject p : objs

-- | Keeps function name and arguments
data Client a where
    Client :: MessagePack a => String -> [Object] -> Client a

instance MessagePack o => RpcType (Client o) where
    rpcc name args = Client name (reverse args)

-- * Server part

-- | Keeps method definition
data Method m = Method
    { methodName :: String
    , methodBody :: [Object] -> m Object
    }

-- | Creates method available for RPC-requests.
--   It accepts method name (which would be refered by clients)
--   and it's body
method :: S.MethodType m f => String -> f -> Method m
method name f = Method
    { methodName = name
    , methodBody = S.toBody f
    }

instance Monad m => S.MethodType m Object where
    toBody res [] = return res
    toBody _   _  = error "Too many arguments!"

-- | Helps restrict method type
serverTypeRestriction0 :: Monad m => m (S.ServerT m a -> S.ServerT m a)
serverTypeRestriction0 = return id

serverTypeRestriction1 :: Monad m => m ((b -> S.ServerT m a) -> (b -> S.ServerT m a))
serverTypeRestriction1 = return id

serverTypeRestriction2 :: Monad m => m ((c -> b -> S.ServerT m a) -> (c -> b -> S.ServerT m a))
serverTypeRestriction2 = return id

serverTypeRestriction3 :: Monad m => m ((d -> c -> b -> S.ServerT m a) -> (d -> c -> b -> S.ServerT m a))
serverTypeRestriction3 = return id

serverTypeRestriction4
    :: Monad m
    => m ((e -> d -> c -> b -> S.ServerT m a) -> (e -> d -> c -> b -> S.ServerT m a))
serverTypeRestriction4 = return id

serverTypeRestriction5
    :: Monad m
    => m ((f -> e -> d -> c -> b -> S.ServerT m a) -> (f -> e -> d -> c -> b -> S.ServerT m a))
serverTypeRestriction5 = return id
