{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Specialization of Logging module for RSCoin.

module RSCoin.Core.Logging
       ( module RSCoin.Util.Logging
       , initLogging

         -- * Predefined logger names
       , bankLoggerName
       , benchLoggerName
       , communicationLoggerName
       , explorerLoggerName
       , mintetteLoggerName
       , nakedLoggerName
       , notaryLoggerName
       , testingLoggerName
       , timedLoggerName
       , userLoggerName
       ) where

import           RSCoin.Util.Logging hiding (initLogging)
import qualified RSCoin.Util.Logging as L (initLogging)

initLogging :: Severity -> IO ()
initLogging = L.initLogging predefinedLoggers

bankLoggerName,
    benchLoggerName,
    communicationLoggerName,
    explorerLoggerName,
    mintetteLoggerName,
    nakedLoggerName,
    notaryLoggerName,
    testingLoggerName,
    timedLoggerName,
    userLoggerName :: LoggerName
bankLoggerName          = "bank"
benchLoggerName         = "bench"
communicationLoggerName = "communication"
explorerLoggerName      = "explorer"
mintetteLoggerName      = "mintette"
nakedLoggerName         = "naked"
notaryLoggerName        = "notary"
testingLoggerName       = "testing"
timedLoggerName         = "timed"
userLoggerName          = "user"

predefinedLoggers :: [LoggerName]
predefinedLoggers =
    [ bankLoggerName
    , communicationLoggerName
    , explorerLoggerName
    , mintetteLoggerName
    , nakedLoggerName
    , notaryLoggerName
    , timedLoggerName
    , userLoggerName
    ]

instance WithNamedLogger IO where
    getLoggerName = pure nakedLoggerName
