{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module provides configuration context for running nodes of rscoin.

module RSCoin.Core.NodeConfig
        ( Host
        , NetworkAddress
        , NodeContext (..)
        , Port

          -- * 'NodeContext' lenses
        , bankAddr
        , bankPublicKey
        , ctxLoggerName
        , notaryAddr

          -- * Other lenses
        , bankHost
        , bankPort
        , genesisAddress
        , notaryPort

          -- * Hardcoded constants for tests and benchmarks
        , defaultNodeContext
        , defaultNodeContextWithLogger
        , testBankPublicKey
        , testBankSecretKey

          -- * Functions to read context from configuration file
        , readDeployNodeContext

          -- * ContextArgument and related
        , ContextArgument (..)
        , mkNodeContext

          -- * Type class
        , WithNodeContext (..)
        ) where

import           Control.Exception          (Exception, throwIO)
import           Control.Lens               (Getter, makeLenses, to, (.~), _1,
                                             _2)
import           Control.Monad              (when)
import           Control.Monad.Except       (ExceptT)
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.State        (StateT)
import           Control.Monad.Trans        (lift)

import qualified Data.Configurator          as Config
import qualified Data.Configurator.Types    as Config
import           Data.Maybe                 (fromMaybe, isNothing)
import           Data.String                (IsString)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)

import           Formatting                 (build, sformat, stext, (%))

import           Control.TimeWarp.Rpc       (Host, NetworkAddress, Port)

import           RSCoin.Core.Constants      (defaultConfigurationPath,
                                             defaultPort, localhost)
import           RSCoin.Core.Crypto.Signing (PublicKey, SecretKey,
                                             constructPublicKey,
                                             derivePublicKey,
                                             deterministicKeyGen)
import           RSCoin.Core.Logging        (LoggerName, nakedLoggerName)
import           RSCoin.Core.Primitives     (Address (..))


data NodeContext = NodeContext
    { _bankAddr      :: NetworkAddress
    , _notaryAddr    :: NetworkAddress
    , _bankPublicKey :: PublicKey
    , _ctxLoggerName :: LoggerName
    } deriving (Show)

$(makeLenses ''NodeContext)

-- | Default node context for local deployment.
defaultNodeContext :: NodeContext
defaultNodeContext = defaultNodeContextWithLogger nakedLoggerName

-- | Default node context for local deployment with given logger name.
defaultNodeContextWithLogger :: LoggerName -> NodeContext
defaultNodeContextWithLogger _ctxLoggerName = NodeContext {..}
  where
    _bankAddr      = (localhost, defaultPort)
    _notaryAddr    = (localhost, 4001)
    _bankPublicKey = testBankPublicKey

bankHost :: Getter NodeContext Host
bankHost = bankAddr . _1

bankPort :: Getter NodeContext Port
bankPort = bankAddr . _2

notaryPort :: Getter NodeContext Port
notaryPort = notaryAddr . _2

-- | Special address used as output in genesis transaction
genesisAddress :: Getter NodeContext Address
genesisAddress = bankPublicKey . to Address

-- | This Bank public key should be used only for tests and benchmarks.
testBankPublicKey :: PublicKey
testBankPublicKey = derivePublicKey testBankSecretKey

-- | This Bank secret key should be used only for tests and benchmarks.
testBankSecretKey :: SecretKey
testBankSecretKey = snd $
                    fromMaybe (error "[FATAL] Failed to construct (pk, sk) pair") $
                    deterministicKeyGen "default-node-context-keygen-seed"

bankPublicKeyPropertyName :: IsString s => s
bankPublicKeyPropertyName = "bank.publicKey"

readRequiredDeployContext :: Maybe FilePath -> IO (Config.Config, NodeContext)
readRequiredDeployContext configPath = do
    confFile <- defaultConfigurationPath
    deployConfig <-
        Config.load [Config.Required (fromMaybe confFile configPath)]

    cfgBankHost <- Config.require deployConfig "bank.host"
    cfgBankPort <- Config.require deployConfig "bank.port"
    cfgNotaryHost <- Config.require deployConfig "notary.host"
    cfgNotaryPort <- Config.require deployConfig "notary.port"

    let obtainedContext =
            defaultNodeContext
            { _bankAddr = (cfgBankHost, cfgBankPort)
            , _notaryAddr = (cfgNotaryHost, cfgNotaryPort)
            }
    return (deployConfig, obtainedContext)

data ConfigurationReadException
    = ConfigurationReadException T.Text
    deriving (Show, Typeable)

instance Exception ConfigurationReadException

-- | Reads config from 'defaultConfigurationPath' and converts into 'NodeContext'.
-- Tries to read also bank public key if it is not provided. If provied then throws
-- exception in case of mismatch.
readDeployNodeContext :: Maybe SecretKey -> Maybe FilePath -> IO NodeContext
readDeployNodeContext (Just newBankSecretKey) confPath = do
    (deployConfig, obtainedContext) <- readRequiredDeployContext confPath

    cfgBankPublicKey <- Config.lookup deployConfig bankPublicKeyPropertyName
    when (isNothing cfgBankPublicKey)
        $ throwIO $ ConfigurationReadException
        $ sformat ("Configuration file doesn't have property: " % stext) bankPublicKeyPropertyName

    let Just cfgReadPublicKey = cfgBankPublicKey
    let newBankPublicKey      = derivePublicKey newBankSecretKey
    let pkConfigValue         = Config.String $ sformat build newBankPublicKey
    when (pkConfigValue /= cfgReadPublicKey)
        $ throwIO $ ConfigurationReadException
        $ sformat ("Bank's derived PK " % build % " doesn't match PK in cfg file") newBankPublicKey

    return obtainedContext
        { _bankPublicKey = newBankPublicKey
        }
readDeployNodeContext Nothing confPath = do
    (deployConfig, obtainedContext) <- readRequiredDeployContext confPath
    cfgBankPublicKey  <- Config.require deployConfig bankPublicKeyPropertyName
    case constructPublicKey cfgBankPublicKey of
        Nothing -> throwIO
            $ ConfigurationReadException
            $ sformat (stext % " is not a valid public key in config file") cfgBankPublicKey
        Just pk ->
            return obtainedContext { _bankPublicKey = pk }

-- | ContextArgument is passed to functions which need NodeContext. It
-- aggregates variety of ways to pass context.
data ContextArgument
    = CAExisting NodeContext     -- ^ Existing NodeContext -- will be used.
    | CADefaultLocation          -- ^ Context will be read from default location.
    | CACustomLocation FilePath  -- ^ Context will be read from given location.
    | CADefault                  -- ^ Default context will be used.
    deriving (Show)

mkNodeContext :: LoggerName
              -> Maybe SecretKey
              -> ContextArgument
              -> IO NodeContext
mkNodeContext loggerName sk ca =
    (ctxLoggerName .~ loggerName $) <$> mkNodeContextDo sk ca

mkNodeContextDo :: Maybe SecretKey -> ContextArgument -> IO NodeContext
mkNodeContextDo _ (CAExisting ctx) = pure ctx
mkNodeContextDo bankSecretKey CADefaultLocation =
    readDeployNodeContext bankSecretKey Nothing
mkNodeContextDo bankSecretKey (CACustomLocation p) =
    readDeployNodeContext bankSecretKey (Just p)
mkNodeContextDo _ CADefault = pure defaultNodeContext

class WithNodeContext m where
    getNodeContext :: m NodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (ReaderT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (StateT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (ExceptT e m) where
    getNodeContext = lift getNodeContext
