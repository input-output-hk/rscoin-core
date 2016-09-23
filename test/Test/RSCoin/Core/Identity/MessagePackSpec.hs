{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.Identity.MessagePackSpec
       ( spec
       , mid
       ) where

import           Data.Int              (Int64)
import           Data.MessagePack      (MessagePack (..), pack, unpack)
import           Data.Proxy            (Proxy (Proxy))
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary, (===))

import qualified RSCoin.Core           as C

spec :: Spec
spec =
    describe "MessagePack" $ do
        describe "Identity Properties" $ do
            makeMsgPackProp "Coin" (Proxy :: Proxy C.Coin)
            makeMsgPackProp "Signature" (Proxy :: Proxy (C.Signature Int))
            makeMsgPackProp "Address" (Proxy :: Proxy C.Address)
            makeMsgPackProp "Mintette" (Proxy :: Proxy C.Mintette)
            makeMsgPackProp "Hash" (Proxy :: Proxy (C.Hash Int))
            makeMsgPackProp "Explorer" (Proxy :: Proxy C.Explorer)
            makeMsgPackProp "NewPeriodData"
                (Proxy :: Proxy C.SmallNewPeriodData)
            makeMsgPackProp "LBlock" (Proxy :: Proxy C.SmallLBlock)
            makeMsgPackProp "Transaction"
                (Proxy :: Proxy C.SmallTransaction)
            makeMsgPackProp "CheckConfirmation"
                (Proxy :: Proxy C.CheckConfirmation)
            makeMsgPackProp "CommitAcknowledgment"
                (Proxy :: Proxy C.CommitAcknowledgment)
            makeMsgPackProp "HBlock" (Proxy :: Proxy C.SmallHBlock)
            makeMsgPackProp "TxStrategy" (Proxy :: Proxy C.SmallTxStrategy)
            makeMsgPackProp "PartyAddress" (Proxy :: Proxy C.PartyAddress)
            makeMsgPackProp "AllocationAddress"
                (Proxy :: Proxy C.AllocationAddress)
            makeMsgPackProp "AllocationStrategy"
                (Proxy :: Proxy C.SmallAllocationStrategy)
            makeMsgPackProp "AllocationInfo"
                (Proxy :: Proxy C.SmallAllocationInfo)
            makeMsgPackProp "ActionLogEntry"
                (Proxy :: Proxy C.SmallActionLogEntry)
            makeMsgPackProp "BankLocalControlRequest"
                (Proxy :: Proxy C.BankLocalControlRequest)
            makeMsgPackProp "HBlockMetadata" (Proxy :: Proxy C.HBlockMetadata)
            makeMsgPackProp "WithMetadata"
                (Proxy :: Proxy (C.WithMetadata Int64 String))
            makeMsgPackProp "WithSignature"
                (Proxy :: Proxy (C.WithSignature String))

makeMsgPackProp
    :: forall a.
       (Show a, Eq a, MessagePack a, Arbitrary a)
    => String -> Proxy a -> Spec
makeMsgPackProp s Proxy = prop s $ \(x :: a) -> x === mid x

mid :: MessagePack a => a -> a
mid = maybe err id . unpack . pack
  where
    err = error "[MessagePackSpec] Failed MessagePack unpacking!"
