{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.ShowReadSpec
       ( spec
       ) where

import           Data.Proxy            (Proxy (Proxy))

import           RSCoin.Core           (PublicKey, SecretKey, Signature)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary, (===))


spec :: Spec
spec =
    describe "ShowRead" $
        describe "Identity Properties" $ do
            makeShowReadProp "Signature" (Proxy :: Proxy (Signature Int))
            makeShowReadProp "SecretKey" (Proxy :: Proxy SecretKey)
            makeShowReadProp "PublicKey" (Proxy :: Proxy PublicKey)

makeShowReadProp
    :: forall a.
       (Show a, Read a, Eq a, Arbitrary a)
    => String -> Proxy a -> Spec
makeShowReadProp s Proxy = prop s $ \(x :: a) -> x === showMid x

showMid :: (Show a, Read a) => a -> a
showMid = read . show
