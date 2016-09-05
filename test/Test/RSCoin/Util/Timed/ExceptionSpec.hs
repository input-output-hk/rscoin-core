{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.RSCoin.Util.Timed.ExceptionSpec
       ( spec
       ) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO,
                                              readTVarIO)
import           Control.Exception.Base      (ArithException (Overflow),
                                              AsyncException (ThreadKilled),
                                              SomeException)
import           Control.Monad.Catch         (catch, catchAll, throwM)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans         (MonadIO)
import           Test.Hspec                  (Spec, before, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (NonNegative (..), Property)
import           Test.QuickCheck.Monadic     (monadicIO)
import           Test.QuickCheck.Property    (Result (reason), failed,
                                              ioProperty, succeeded)

import           RSCoin.Util.Logging         (Severity (Error),
                                              WithNamedLogger (getLoggerName),
                                              initLogging)
import           RSCoin.Util.Timed           (Microsecond, TimedT, after, for,
                                              fork_, invoke, runTimedT, sec,
                                              wait)


instance WithNamedLogger IO where
    getLoggerName = pure "dunno"

spec :: Spec
spec =
    before (initLogging ["dunno"] Error) $
    describe "WorkMode" $ do
        describe "error" $ do
            prop "should abort the execution"
                exceptionShouldAbortExecution
            prop "caught nicely"
                excCaught
            prop "exception from main thread caught outside of monad"
                excCaughtOutside
            prop "exception from main thread caught outside of monad (+ wait)"
                excCaughtOutsideWithWait
            prop "proper catch order (catch inside catch)"
                excCatchOrder
            prop "(wait + throw) - exception doesn't get lost"
                excWaitThrow
            prop "(wait + throw) in forked thread - exception doesn't get lost"
                excWaitThrowForked
            prop "catch doesn't handle future exceptions"
                excCatchScope
            prop "catch doesn't handle future exceptions (with wait inside)"
                excCatchScopeWithWait
            prop "different exceptions, catch inner"
                excDiffCatchInner
            prop "different exceptions, catch outer"
                excDiffCatchOuter
        describe "handler error" $
            prop "throws"
                handlerThrow
        describe "async error" $
            prop "shouldn't abort the execution"
                asyncExceptionShouldntAbortExecution

-- FIXME
exceptionShouldAbortExecution
    :: NonNegative Microsecond
    -> Property
exceptionShouldAbortExecution (getNonNegative -> t) = (const (monadicIO $ pure ())) t
    -- monadicIO $
    -- do var <- liftIO $ newTVarIO (0 :: Int)
    --    runTimedT $
    --        fork_ $
    --        do liftIO $ atomically $ writeTVar var 1
    --           wait $ for t mcs
    --           void $ throwM $ TextException "Error"
    --           liftIO $ atomically $ writeTVar var 2
    --    res <- liftIO $ readTVarIO var
    --    assert $ res == 1

-- FIXME
asyncExceptionShouldntAbortExecution
    :: NonNegative Microsecond
    -> NonNegative Microsecond
    -> Property
asyncExceptionShouldntAbortExecution (getNonNegative -> t1) (getNonNegative -> t2) = (const (const (monadicIO $ pure ()))) t1 t2
--     monadicIO $
--     do var <- liftIO $ newTVarIO (0 :: Int)
--        runTimedT $
--            do liftIO $ atomically $ writeTVar var 1
--               fork_ $
--                   do wait $ for t2 mcs
--                      throwM $ TextException "Error"
--               wait $ for t1 mcs
--               liftIO $ atomically $ writeTVar var 2
--        res <- liftIO $ readTVarIO var
--        assert $ res == 2

excCaught
    :: Property
excCaught =
    ioProperty . withCheckPoints $
        \checkPoint -> runEmu $
            let act   = throwM ThreadKilled >> checkPoint (-1)
                hnd _ = checkPoint 1
            in  act `catchAll` hnd

excCaughtOutside
    :: Property
excCaughtOutside =
    ioProperty . withCheckPoints $
        \checkPoint ->
            let act = do
                    runEmu $ wait (for 1 sec) >> throwM ThreadKilled
                    checkPoint (-1)
                hnd _ = checkPoint 1
            in  do  act `catchAll` hnd
                    checkPoint 2

excCaughtOutsideWithWait
    :: Property
excCaughtOutsideWithWait =
    ioProperty . withCheckPoints $
    \checkPoint ->
         let act = do
                 runEmu $ throwM ThreadKilled
                 checkPoint (-1)
             hnd _ = checkPoint 1
         in do act `catchAll` hnd
               checkPoint 2

excWaitThrow
    :: Property
excWaitThrow =
    ioProperty . withCheckPoints $
    \checkPoint ->
         runEmu $
         let act = do
                 wait (for 1 sec)
                 throwM ThreadKilled
             hnd _ = checkPoint 1
         in do act `catchAll` hnd
               checkPoint 2

excWaitThrowForked
    :: Property
excWaitThrowForked =
    ioProperty . withCheckPoints $
    \checkPoint ->
         runEmu $
         let act = do
                 wait (for 1 sec)
                 throwM ThreadKilled
             hnd _ = checkPoint 1
         in do fork_ $ act `catchAll` hnd
               invoke (after 1 sec) $ checkPoint 2

excCatchOrder
    :: Property
excCatchOrder =
    ioProperty . withCheckPoints $
    \checkPoint ->
         runEmu $
         let act = throwM ThreadKilled
             hnd1 _ = checkPoint 1
             hnd2 _ = checkPoint (-1)
         in do act `catchAll` hnd1 `catchAll` hnd2
               checkPoint 2

excCatchScope
    :: Property
excCatchScope =
    ioProperty . withCheckPoints $
    \checkPoint ->
         runEmu $
         let act1 = checkPoint 1 `catchAll` const (checkPoint $ -1)
             act2 = act1 >> throwM ThreadKilled
         in do act2 `catchAll` const (checkPoint 2)
               checkPoint 3

excCatchScopeWithWait :: Property
excCatchScopeWithWait =
    ioProperty . withCheckPoints $
    \checkPoint ->
         runEmu $
         let act1 = checkPoint 1 >> wait (for 1 sec)
             act2 = act1 `catchAll` const (checkPoint $ -1)
             act3 = act2 >> wait (for 1 sec) >> throwM ThreadKilled
         in do act3 `catchAll` const (checkPoint 2)
               checkPoint 3

excDiffCatchInner
    :: Property
excDiffCatchInner =
    ioProperty . withCheckPoints $
    \checkPoint ->
         runEmu $
         let act = throwM ThreadKilled
             hnd1 (_ :: AsyncException) = checkPoint 1
             hnd2 (_ :: ArithException) = checkPoint (-1)
         in do act `catch` hnd1 `catch` hnd2
               checkPoint 2

excDiffCatchOuter
    :: Property
excDiffCatchOuter =
    ioProperty . withCheckPoints $
    \checkPoint ->
         runEmu $
         let act = throwM Overflow
             hnd1 (_ :: AsyncException) = checkPoint (-1)
             hnd2 (_ :: ArithException) = checkPoint 1
         in do act `catch` hnd1 `catch` hnd2
               checkPoint 2

handlerThrow
    :: Property
handlerThrow =
    ioProperty . withCheckPoints $
    \checkPoint ->
         runEmu $
         let act = throwM ThreadKilled
             hnd1 (_ :: SomeException) = throwM Overflow
             hnd2 (_ :: ArithException) = checkPoint 1
         in do act `catch` hnd1 `catch` hnd2
               checkPoint 2


runEmu :: TimedT IO () -> IO ()
runEmu = runTimedT

-- Principle of checkpoints: every checkpoint has it's id
-- Checkpoints should be visited in according order: 1, 2, 3 ...
newtype CheckPoints = CP { getCP :: TVar (Either String Int) }

initCheckPoints :: MonadIO m => m CheckPoints
initCheckPoints = fmap CP $ liftIO $ newTVarIO $ Right 0

visitCheckPoint :: MonadIO m => CheckPoints -> Int -> m ()
visitCheckPoint cp curId = liftIO $ atomically $ modifyTVar (getCP cp) $
    \wasId ->
        if wasId == Right (curId - 1)
            then Right curId
            else Left $ either id (showError curId) wasId
  where
    showError cur was = mconcat
        ["Wrong chechpoint. Expected "
        , show (was + 1)
        , ", but visited "
        , show cur
        ]

assertCheckPoints :: MonadIO m => CheckPoints -> m Result
assertCheckPoints = fmap mkRes . liftIO . readTVarIO . getCP
  where
    mkRes (Left msg) = failed { reason = msg }
    mkRes (Right _)  = succeeded

withCheckPoints :: MonadIO m => ((Int -> m ()) -> IO a) -> IO Result
withCheckPoints act = do
    cp <- initCheckPoints
    _  <- act $ liftIO . visitCheckPoint cp
    assertCheckPoints cp
