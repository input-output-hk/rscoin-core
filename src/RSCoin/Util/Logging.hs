{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Logging functionality.

module RSCoin.Util.Logging
       ( Severity (..)
       , initLogging
       , initLoggerByName

       , LoggerName (..)

         -- * Logging functions
       , WithNamedLogger (..)
       , logDebug
       , logError
       , logInfo
       , logMessage
       , logWarning
       ) where

import           Control.Monad.Except      (ExceptT)
import           Control.Monad.Reader      (MonadReader (ask), ReaderT)
import           Control.Monad.State       (StateT)
import           Control.Monad.Trans       (MonadIO (liftIO), lift)

import           Data.String               (IsString)
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)

import           System.Console.ANSI       (Color (Blue, Green, Red, Yellow),
                                            ColorIntensity (Vivid),
                                            ConsoleLayer (Foreground),
                                            SGR (Reset, SetColor), setSGRCode)
import           System.IO                 (stderr, stdout)
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger         (Priority (DEBUG, ERROR, INFO, WARNING),
                                            logM, removeHandler, rootLoggerName,
                                            setHandlers, setLevel,
                                            updateGlobalLogger)

-- | This type is intended to be used as command line option
-- which specifies which messages to print.
data Severity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Generic, Typeable, Show, Read, Eq)

newtype LoggerName = LoggerName
    { loggerName :: String
    } deriving (Show, IsString)

convertSeverity :: Severity -> Priority
convertSeverity Debug   = DEBUG
convertSeverity Info    = INFO
convertSeverity Warning = WARNING
convertSeverity Error   = ERROR

initLogging :: [LoggerName] -> Severity -> IO ()
initLogging predefinedLoggers sev = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    mapM_ (initLoggerByName sev) predefinedLoggers

initLoggerByName :: Severity -> LoggerName -> IO ()
initLoggerByName (convertSeverity -> s) LoggerName{..} = do
    stdoutHandler <-
        (flip setFormatter) stdoutFormatter <$> streamHandler stdout s
    stderrHandler <-
        (flip setFormatter) stderrFormatter <$> streamHandler stderr ERROR
    updateGlobalLogger loggerName $ setHandlers [stdoutHandler, stderrHandler]
  where
    stderrFormatter = simpleLogFormatter
        ("[$time] " ++ colorizer ERROR "[$loggername:$prio]: " ++ "$msg")
    stdoutFormatter h r@(pr, _) n =
        simpleLogFormatter (colorizer pr "[$loggername:$prio] " ++ "$msg") h r n

table :: Priority -> (String, String)
table priority = case priority of
    ERROR   -> (setColor Red   , reset)
    DEBUG   -> (setColor Green , reset)
    WARNING -> (setColor Yellow, reset)
    INFO    -> (setColor Blue  , reset)
    _       -> ("", "")
  where
    setColor color = setSGRCode [SetColor Foreground Vivid color]
    reset = setSGRCode [Reset]

colorizer :: Priority -> String -> String
colorizer pr s = before ++ s ++ after
  where
    (before, after) = table pr

-- | This type class exists to remove boilerplate logging
-- by adding the logger's name to the environment in each module.
class WithNamedLogger m where
    getLoggerName :: m LoggerName

instance (Monad m, WithNamedLogger m) =>
         WithNamedLogger (ReaderT a m) where
    getLoggerName = lift getLoggerName

instance (Monad m, WithNamedLogger m) =>
         WithNamedLogger (StateT a m) where
    getLoggerName = lift getLoggerName

instance (Monad m, WithNamedLogger m) =>
         WithNamedLogger (ExceptT e m) where
    getLoggerName = lift getLoggerName

instance {-# OVERLAPPABLE #-} MonadReader LoggerName m =>
         WithNamedLogger m where
    getLoggerName = ask

logDebug :: (WithNamedLogger m, MonadIO m)
         => T.Text -> m ()
logDebug = logMessage Debug

logInfo :: (WithNamedLogger m, MonadIO m)
        => T.Text -> m ()
logInfo = logMessage Info

logWarning :: (WithNamedLogger m, MonadIO m)
        => T.Text -> m ()
logWarning = logMessage Warning

logError :: (WithNamedLogger m, MonadIO m)
         => T.Text -> m ()
logError = logMessage Error

logMessage
    :: (WithNamedLogger m, MonadIO m)
    => Severity -> T.Text -> m ()
logMessage severity t = do
    LoggerName{..} <- getLoggerName
    liftIO . logM loggerName (convertSeverity severity) $ T.unpack t
