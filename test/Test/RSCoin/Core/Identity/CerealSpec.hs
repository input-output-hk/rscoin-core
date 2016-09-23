{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.Identity.CerealSpec
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
    describe "SafeCopy" $ do
        describe "Identity Properties" $ do
            makeSafeCopyProp "Hash" (Proxy :: Proxy (C.Hash Int))
            makeSafeCopyProp "Mintette" (Proxy :: Proxy C.Mintette)
            makeSafeCopyProp "Explorer" (Proxy :: Proxy C.Explorer)
            makeSafeCopyProp "CheckConfirmation"
                (Proxy :: Proxy C.CheckConfirmation)
            makeSafeCopyProp "CommitAcknowledgment"
                (Proxy :: Proxy C.CommitAcknowledgment)
            makeSafeCopyProp "ActionLogEntry"
                (Proxy :: Proxy C.SmallActionLogEntry)
            makeSafeCopyProp "LBlock" (Proxy :: Proxy C.SmallLBlock)
            makeSafeCopyProp "HBlock" (Proxy :: Proxy C.SmallHBlock)
            makeSafeCopyProp "PeriodResult" (Proxy :: Proxy C.SmallPeriodResult)
            makeSafeCopyProp "NewPeriodData"
                (Proxy :: Proxy C.SmallNewPeriodData)
            makeSafeCopyProp "HBlockMetada" (Proxy :: Proxy C.HBlockMetadata)
            makeSafeCopyProp "WithMetadata"
                (Proxy :: Proxy (C.WithMetadata Int Int))
            makeSafeCopyProp "WithSignature"
                (Proxy :: Proxy (C.WithSignature Int))
            makeSafeCopyProp "TxStrategy" (Proxy :: Proxy C.SmallTxStrategy)
            makeSafeCopyProp "AllocationAddress"
                (Proxy :: Proxy C.AllocationAddress)
            makeSafeCopyProp "PartyAddress" (Proxy :: Proxy C.PartyAddress)
            makeSafeCopyProp "AllocationStrategy"
                (Proxy :: Proxy C.AllocationStrategy)
            makeSafeCopyProp "AllocationInfo"
                (Proxy :: Proxy C.SmallAllocationInfo)
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
