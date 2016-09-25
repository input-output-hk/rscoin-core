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
        , notaryAddr
        , notaryPublicKey

          -- * Other lenses
        , bankHost
        , bankPort
        , genesisAddress
        , notaryPort

          -- * Hardcoded constants for tests and benchmarks
        , defaultNodeContext
        , testBankPublicKey
        , testBankSecretKey
        , testNotaryPublicKey
        , testNotarySecretKey

          -- * Functions to read context from configuration file
        , readDeployNodeContext

          -- * ContextArgument and related
        , ContextArgument (..)
        , mkNodeContext

          -- * Type class
        , WithNodeContext (..)

          -- * Extra helpers
        , isTestRun
        ) where

import           Control.Applicative        (liftA2)
import           Control.Exception          (Exception, throwIO)
import           Control.Lens               (Getter, makeLenses, to, view,
                                             _1, _2)
import           Control.Monad              (mzero, when)
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

import           Control.TimeWarp.Rpc       (Host, NetworkAddress, Port,
                                             ServerT)

import           RSCoin.Core.Constants      (defaultConfigurationPath,
                                             defaultPort, localhost)
import           RSCoin.Core.Crypto.Signing (PublicKey, SecretKey,
                                             constructPublicKey,
                                             derivePublicKey,
                                             deterministicKeyGen)
import           RSCoin.Core.Primitives     (Address (..))


data NodeContext = NodeContext
    { _bankAddr        :: !NetworkAddress
    , _notaryAddr      :: !NetworkAddress
    , _bankPublicKey   :: !PublicKey
    , _notaryPublicKey :: !PublicKey
    } deriving (Show)

$(makeLenses ''NodeContext)

-- | Default node context for local deployment.
defaultNodeContext :: NodeContext
defaultNodeContext = NodeContext {..}
  where
    _bankAddr      = (localhost, defaultPort)
    _notaryAddr    = (localhost, 4001)
    _bankPublicKey = testBankPublicKey
    _notaryPublicKey = testNotaryPublicKey

bankHost :: Getter NodeContext Host
bankHost = bankAddr . _1

bankPort :: Getter NodeContext Port
bankPort = bankAddr . _2

notaryPort :: Getter NodeContext Port
notaryPort = notaryAddr . _2

-- | Special address used as output in genesis transaction
genesisAddress :: Getter NodeContext Address
genesisAddress = bankPublicKey . to Address

-- | This Bank public key should be used only for tests, benchmarks
-- and local deployment.
testBankPublicKey :: PublicKey
testBankPublicKey = derivePublicKey testBankSecretKey

-- | This Bank secret key should be used only for tests, benchmarks
-- and local deployment.
testBankSecretKey :: SecretKey
testBankSecretKey =
    snd $
    fromMaybe (error "[FATAL] Failed to construct (pk, sk) pair") $
    deterministicKeyGen "default-node-context-keygen-seed"

-- | This Notary public key should be used only for tests, benchmarks
-- and local deployment.
testNotaryPublicKey :: PublicKey
testNotaryPublicKey = derivePublicKey testNotarySecretKey

-- | This Notary secret key should be used only for tests, benchmarks
-- and local deployment.
testNotarySecretKey :: SecretKey
testNotarySecretKey =
    snd $
    fromMaybe (error "[FATAL] Failed to construct (pk, sk) pair for Notary") $
    deterministicKeyGen "dees-negyek-txetnoc-edon-tluafed"

bankPublicKeyPropertyName :: IsString s => s
bankPublicKeyPropertyName = "bank.publicKey"

notaryPublicKeyPropertyName :: IsString s => s
notaryPublicKeyPropertyName = "notary.publicKey"

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

instance Config.Configured PublicKey where
    convert (Config.String text) = constructPublicKey text
    convert _                    = mzero

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

    cfgNotaryPublicKey <- Config.require deployConfig notaryPublicKeyPropertyName

    let Just cfgReadPublicKey = cfgBankPublicKey
    let newBankPublicKey      = derivePublicKey newBankSecretKey
    let pkConfigValue         = Config.String $ sformat build newBankPublicKey
    when (pkConfigValue /= cfgReadPublicKey)
        $ throwIO $ ConfigurationReadException
        $ sformat ("Bank's derived PK " % build % " doesn't match PK in cfg file") newBankPublicKey

    return obtainedContext
        { _bankPublicKey = newBankPublicKey
        , _notaryPublicKey = cfgNotaryPublicKey
        }
readDeployNodeContext Nothing confPath = do
    (deployConfig, obtainedContext) <- readRequiredDeployContext confPath
    cfgBankPublicKey  <- Config.require deployConfig bankPublicKeyPropertyName
    cfgNotaryPublicKey <- Config.require deployConfig notaryPublicKeyPropertyName
    case constructPublicKey cfgBankPublicKey of
        Nothing -> throwIO
            $ ConfigurationReadException
            $ sformat (stext % " is not a valid public key in config file") cfgBankPublicKey
        Just pk ->
            return obtainedContext { _bankPublicKey = pk
                                   , _notaryPublicKey = cfgNotaryPublicKey
                                   }

-- | ContextArgument is passed to functions which need NodeContext. It
-- aggregates variety of ways to pass context.
data ContextArgument
    = CAExisting NodeContext     -- ^ Existing NodeContext -- will be used.
    | CADefaultLocation          -- ^ Context will be read from default location.
    | CACustomLocation FilePath  -- ^ Context will be read from given location.
    | CADefault                  -- ^ Default context will be used.
    deriving (Show)

mkNodeContext :: Maybe SecretKey
              -> ContextArgument
              -> IO NodeContext
mkNodeContext _ (CAExisting ctx) = pure ctx
mkNodeContext bankSecretKey CADefaultLocation =
    readDeployNodeContext bankSecretKey Nothing
mkNodeContext bankSecretKey (CACustomLocation p) =
    readDeployNodeContext bankSecretKey (Just p)
mkNodeContext _ CADefault = pure defaultNodeContext

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

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (ServerT m) where
    getNodeContext = lift getNodeContext

-- | Returns True iff bank's and notary's keys in context are test keys.
isTestRun
    :: (Applicative m, WithNodeContext m)
    => m Bool
isTestRun =
    ((testBankPublicKey, testNotaryPublicKey) ==) <$>
    (liftA2 (,) (view bankPublicKey) (view notaryPublicKey) <$> getNodeContext)
