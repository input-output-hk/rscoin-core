{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.CerealSpec
       ( spec
       ) where

import           Data.Proxy            (Proxy (Proxy))
import           Data.SafeCopy         (SafeCopy, safeGet, safePut)
import           Data.Serialize        (Serialize, decode, encode, runGet,
                                        runPut)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary, (===))

import qualified RSCoin.Core           as C

spec :: Spec
spec = do
    describe "Serialize" $ do
        describe "Identity Properties" $ do
            makeCerealProp "Integer" (Proxy :: Proxy Integer)
            makeCerealProp "Rational" (Proxy :: Proxy Rational)
            makeCerealProp "Either Int Int" (Proxy :: Proxy (Either Int Int))
            -- makeCerealProp "Coin" (Proxy :: Proxy C.Coin)
            -- prop "Signature" $
            --     \(a :: C.Signature Int) -> a === binMid a
            -- prop "Address" $
            --     \(a :: C.Address) -> a === binMid a
            -- prop "Mintette" $
            --     \(a :: C.Mintette) -> a === binMid a
            -- prop "Hash" $
            --     \(a :: C.Hash Int) -> a === binMid a
            -- prop "Explorer" $
            --     \(a :: C.Explorer) -> a === binMid a
            -- {-prop "SmallNewPeriodData" $
            --     \(a :: SmallNewPeriodData) -> a === binMid a-}
            -- {-prop "SmallLBlock" $
            --     \(a :: SmallLBlock) -> a === binMid a-}
            -- prop "Transaction" $
            --     \(a :: C.Transaction) -> a === binMid a
            -- {-prop "SmallTransaction" $
            --     \(a :: SmallTransaction) -> a === binMid a-}
            -- prop "CheckConfirmation" $
            --     \(a :: C.CheckConfirmation) -> a === binMid a
            -- prop "CommitAcknowledgment" $
            --     \(a :: C.CommitAcknowledgment) -> a === binMid a
            -- prop "HBlock" $
            --     \(a :: C.HBlock) -> a === binMid a
            -- {-prop "SmallHBlock" $
            --     \(a :: SmallHBlock) -> a === binMid a-}
            -- prop "TxStrategy" $
            --     \(a :: C.TxStrategy) -> a === binMid a
            -- prop "PartyAddress" $
            --     \(a :: C.PartyAddress) -> a === binMid a
            -- prop "AllocationAddress" $
            --     \(a :: C.AllocationAddress) -> a === binMid a
            -- prop "AllocationStrategy" $
            --     \(a :: C.AllocationStrategy) -> a === binMid a
            -- prop "ActionLogEntry" $
            --     \(a :: C.ActionLogEntry) -> a === binMid a
    describe "SafeCopy" $ do
        describe "Identity Properties" $ do
            makeSafeCopyProp "Integer" (Proxy :: Proxy Integer)
            makeSafeCopyProp "Rational" (Proxy :: Proxy Rational)
            makeSafeCopyProp "Either Int Int" (Proxy :: Proxy (Either Int Int))
            makeSafeCopyProp "Hash" (Proxy :: Proxy (C.Hash Int))
            {-makeSafeCopyProp "Mintette" (Proxy :: Proxy C.Mintette)
            makeSafeCopyProp "Explorer" (Proxy :: Proxy C.Explorer)
            makeSafeCopyProp "CheckConfirmation"
                (Proxy :: Proxy C.CheckConfirmation)
            makeSafeCopyProp "CommitAcknowledgment"
                (Proxy :: Proxy C.CommitAcknowledgment)
            makeSafeCopyProp "ActionLogEntry"
                (Proxy :: Proxy C.SmallActionLogEntry)-}
            --makeSafeCopyProp "LBlock" (Proxy :: Proxy C.SmallLBlock)
            --makeSafeCopyProp "HBlock" (Proxy :: Proxy C.SmallHBlock)
            makeSafeCopyProp "PeriodResult" (Proxy :: Proxy C.PeriodResult)
            makeSafeCopyProp "NewPeriodData"
                (Proxy :: Proxy C.SmallNewPeriodData)
            makeSafeCopyProp "HBlockMetada" (Proxy :: Proxy C.HBlockMetadata)
            makeSafeCopyProp "WithMetadata"
                (Proxy :: Proxy (C.WithMetadata Int Int))
            makeSafeCopyProp "WithSignature"
                (Proxy :: Proxy (C.WithSignature Int))
            makeSafeCopyProp "TxStrategy" (Proxy :: Proxy C.TxStrategy)
            makeSafeCopyProp "ALlocationAddress"
                (Proxy :: Proxy C.AllocationAddress)
            makeSafeCopyProp "PartyAddress" (Proxy :: Proxy C.PartyAddress)
            makeSafeCopyProp "AllocationStrategy"
                (Proxy :: Proxy C.AllocationStrategy)
            makeSafeCopyProp "AllocationInfo" (Proxy :: Proxy C.AllocationInfo)
            makeSafeCopyProp "SecretKey" (Proxy :: Proxy C.SecretKey)
            makeSafeCopyProp "PublicKey" (Proxy :: Proxy C.PublicKey)
            makeSafeCopyProp "Address" (Proxy :: Proxy C.Address)
            makeSafeCopyProp "Color" (Proxy :: Proxy C.Color)
            makeSafeCopyProp "CoinAmount" (Proxy :: Proxy C.CoinAmount)
            makeSafeCopyProp "Coin" (Proxy :: Proxy C.Coin)
            makeSafeCopyProp "Transaction" (Proxy :: Proxy C.SmallTransaction)

makeCerealProp
    :: forall a.
       (Show a, Eq a, Serialize a, Arbitrary a)
    => String -> Proxy a -> Spec
makeCerealProp s Proxy = prop s $ \(x :: a) -> x === cerealMid x

cerealMid :: Serialize a => a -> a
cerealMid = either error id . decode . encode

makeSafeCopyProp
    :: forall a.
       (Show a, Eq a, SafeCopy a, Arbitrary a)
    => String -> Proxy a -> Spec
makeSafeCopyProp s Proxy = prop s $ \(x :: a) -> x === safeCopyMid x

safeCopyMid :: SafeCopy a => a -> a
safeCopyMid = either error id . runGet safeGet . runPut . safePut
