{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.BinarySpec
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
            makeBinaryProp "Integer" (Proxy :: Proxy Integer)
            makeBinaryProp "Rational" (Proxy :: Proxy Rational)
            makeBinaryProp "Either Int Int" (Proxy :: Proxy (Either Int Int))
            makeBinaryProp "Either Int (Either Int Int)"
                (Proxy :: Proxy (Either Int (Either Int Int)))
            makeBinaryProp "Either (Either Int Int) Int"
                (Proxy :: Proxy (Either (Either Int Int) Int))
            {-makeBinaryProp "Either MintetteError CheckConfirmation"
                (Proxy :: Proxy (Either MintetteError C.CheckConfirmation))-}
            makeBinaryProp "Coin" (Proxy :: Proxy C.Coin)
            makeBinaryProp "Signature" (Proxy :: Proxy (C.Signature Int))
            makeBinaryProp "Address" (Proxy :: Proxy C.Address)
            makeBinaryProp "Mintette" (Proxy :: Proxy C.Mintette)
            makeBinaryProp "Hash" (Proxy :: Proxy (C.Hash Int))
            makeBinaryProp "Explorer" (Proxy :: Proxy C.Explorer)
            makeBinaryProp "SmallNewPeriodData"
                (Proxy :: Proxy C.SmallNewPeriodData)
            {-makeBinaryProp "Transaction" (Proxy :: Proxy C.Transaction)-}
            makeBinaryProp "SmallTransaction"
                (Proxy :: Proxy C.SmallTransaction)
            makeBinaryProp "CheckConfirmation"
                (Proxy :: Proxy C.CheckConfirmation)
            makeBinaryProp "CommitAcknowledgment"
                (Proxy :: Proxy C.CommitAcknowledgment)
            {-makeBinaryProp "HBlock" (Proxy :: Proxy C.HBlock)-}
            makeBinaryProp "SmallHBlock" (Proxy :: Proxy C.SmallHBlock)
            makeBinaryProp "TxStrategy" (Proxy :: Proxy C.TxStrategy)
            makeBinaryProp "PartyAddress" (Proxy :: Proxy C.PartyAddress)
            makeBinaryProp "AllocationAddress"
                (Proxy :: Proxy C.AllocationAddress)
            makeBinaryProp "AllocationStrategy"
                (Proxy :: Proxy C.AllocationStrategy)
            makeBinaryProp "ActionLogEntry" (Proxy :: Proxy C.ActionLogEntry)

makeBinaryProp
    :: forall a.
       (Show a, Eq a, Binary a, Arbitrary a)
    => String -> Proxy a -> Spec
makeBinaryProp s Proxy = prop s $ \(x :: a) -> x === binMid x

binMid :: Binary a => a -> a
binMid = decode . encode
