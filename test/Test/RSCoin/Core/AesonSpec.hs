{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.AesonSpec
       ( spec
       ) where

import           Data.Aeson            (FromJSON, ToJSON, decode, encode)
import           Data.Proxy            (Proxy (Proxy))
import           Data.Maybe            (fromJust)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary, (===))

import qualified RSCoin.Core           as C
import           RSCoin.Core.Aeson     ()

spec :: Spec
spec =
    describe "Aeson" $ do
        describe "Identity Properties" $ do
            {-makeAesonProp "Coin" (Proxy :: Proxy C.Coin)-}
            makeAesonProp "Signature" (Proxy :: Proxy (C.Signature Int))
            makeAesonProp "Address" (Proxy  :: Proxy C.Address)
            makeAesonProp "Hash" (Proxy :: Proxy (C.Hash Word))
            {-makeAesonProp "SmallHBlock" (Proxy :: Proxy SmallHBlock)-}
            makeAesonProp "PartyAddress" (Proxy :: Proxy C.PartyAddress)
            makeAesonProp "AllocationAddress"
                (Proxy :: Proxy C.AllocationAddress)
            makeAesonProp "AllocationStrategy"
                (Proxy :: Proxy C.AllocationStrategy)

makeAesonProp
    :: forall a.
       (Show a, Eq a, ToJSON a, FromJSON a, Arbitrary a)
    => String -> Proxy a -> Spec
makeAesonProp s Proxy = prop s $ \(x :: a) -> x === aesonMid x

aesonMid :: (ToJSON a, FromJSON a) => a -> a
aesonMid = fromJust . decode . encode
