{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.Identity.AesonSpec
       ( spec
       ) where

import           Data.Aeson            (FromJSON, ToJSON, decode, encode)
import           Data.Proxy            (Proxy (Proxy))
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary, (===))

import qualified RSCoin.Core           as C
import           RSCoin.Core.Aeson     ()

spec :: Spec
spec =
    describe "Aeson" $ do
        describe "Identity Properties" $ do
            makeAesonProp "Coin" (Proxy :: Proxy C.Coin)
            makeAesonProp "CoinAmount" (Proxy :: Proxy C.CoinAmount)
            makeAesonProp "Color" (Proxy :: Proxy C.Color)
            makeAesonProp "Signature" (Proxy :: Proxy (C.Signature Int))
            makeAesonProp "Address" (Proxy  :: Proxy C.Address)
            makeAesonProp "Hash" (Proxy :: Proxy (C.Hash Word))
            makeAesonProp "PartyAddress" (Proxy :: Proxy C.PartyAddress)
            makeAesonProp "AllocationAddress"
                (Proxy :: Proxy C.AllocationAddress)
            makeAesonProp "AllocationStrategy"
                (Proxy :: Proxy C.SmallAllocationStrategy)
            makeAesonProp "PartyAddress" (Proxy :: Proxy C.PartyAddress)
            makeAesonProp "Transaction" (Proxy :: Proxy C.SmallTransaction)
            makeAesonProp "TxStrategy" (Proxy :: Proxy C.SmallTxStrategy)

makeAesonProp
    :: forall a.
       (Show a, Eq a, ToJSON a, FromJSON a, Arbitrary a)
    => String -> Proxy a -> Spec
makeAesonProp s Proxy = prop s $ \(x :: a) -> x === aesonMid x

aesonMid :: (ToJSON a, FromJSON a) => a -> a
aesonMid = maybe err id . decode . encode
  where
    err = error "[aesonMid] Failed decoding"
