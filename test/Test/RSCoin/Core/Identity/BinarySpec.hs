{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.Identity.BinarySpec
       ( spec
       ) where

import           Data.Binary           (Binary, decode, encode)
import           Data.Proxy            (Proxy (Proxy))
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary, (===))

import qualified RSCoin.Core           as C

spec :: Spec
spec =
    describe "Binary" $ do
        describe "Identity Properties" $ do
            makeBinaryProp "Coin" (Proxy :: Proxy C.Coin)
            makeBinaryProp "Signature" (Proxy :: Proxy (C.Signature Int))
            makeBinaryProp "Address" (Proxy :: Proxy C.Address)
            makeBinaryProp "Mintette" (Proxy :: Proxy C.Mintette)
            makeBinaryProp "Hash" (Proxy :: Proxy (C.Hash Int))
            makeBinaryProp "Explorer" (Proxy :: Proxy C.Explorer)
            makeBinaryProp "NewPeriodData"
                (Proxy :: Proxy C.SmallNewPeriodData)
            makeBinaryProp "SmallTransaction"
                (Proxy :: Proxy C.SmallTransaction)
            makeBinaryProp "CheckConfirmation"
                (Proxy :: Proxy C.CheckConfirmation)
            makeBinaryProp "CommitAcknowledgment"
                (Proxy :: Proxy C.CommitAcknowledgment)
            makeBinaryProp "HBlock" (Proxy :: Proxy C.SmallHBlock)
            makeBinaryProp "TxStrategy" (Proxy :: Proxy C.SmallTxStrategy)
            makeBinaryProp "PartyAddress" (Proxy :: Proxy C.PartyAddress)
            makeBinaryProp "AllocationAddress"
                (Proxy :: Proxy C.AllocationAddress)
            makeBinaryProp "AllocationStrategy"
                (Proxy :: Proxy C.SmallAllocationStrategy)
            makeBinaryProp "ActionLogEntry"
                (Proxy :: Proxy C.SmallActionLogEntry)

makeBinaryProp
    :: forall a.
       (Show a, Eq a, Binary a, Arbitrary a)
    => String -> Proxy a -> Spec
makeBinaryProp s Proxy = prop s $ \(x :: a) -> x === binMid x

binMid :: Binary a => a -> a
binMid = decode . encode
