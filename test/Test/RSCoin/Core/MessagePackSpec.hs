{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.MessagePackSpec
       ( spec
       , mid
       ) where

import           Data.Int              (Int64)
import           Data.MessagePack      (MessagePack (..), pack, unpack)
import qualified Data.Set              as S
import           Data.Proxy            (Proxy (Proxy))
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (arbitrary), Gen, scale,
                                        (===))

import qualified RSCoin.Core           as C

makeSmall :: Gen a -> Gen a
makeSmall = scale f
  where
    -- f = (round . (sqrt :: Double -> Double) . realToFrac . (`div` 3))
    f 0 = 0
    f 1 = 1
    f 2 = 2
    f 3 = 3
    f 4 = 3
    f n
      | n < 0 = n
      | otherwise =
          (round . (sqrt :: Double -> Double) . realToFrac . (`div` 3)) n

newtype SmallLBlock =
    SmallLBlock C.LBlock
    deriving (MessagePack,Show,Eq)

instance Arbitrary SmallLBlock where
    arbitrary = SmallLBlock <$> makeSmall arbitrary

newtype SmallHBlock =
    SmallHBlock C.HBlock
    deriving (MessagePack,Show,Eq)

instance Arbitrary SmallHBlock where
    arbitrary = SmallHBlock <$> makeSmall arbitrary

newtype SmallNewPeriodData =
    SmallNewPeriodData C.NewPeriodData
    deriving (MessagePack,Show,Eq)

instance Arbitrary SmallNewPeriodData where
    arbitrary = SmallNewPeriodData <$> makeSmall arbitrary

newtype SmallTransaction =
    SmallTransaction C.Transaction
    deriving (MessagePack,Show,Eq)

instance Arbitrary SmallTransaction where
    arbitrary = SmallTransaction <$> makeSmall arbitrary

spec :: Spec
spec =
    describe "MessagePack" $ do
        describe "Identity Properties" $ do
            makeMsgPackProp "Int64" (Proxy :: Proxy Int64)
            makeMsgPackProp "Integer" (Proxy :: Proxy Integer)
            makeMsgPackProp "Rational" (Proxy :: Proxy Rational)
            makeMsgPackProp "Either Int Int" (Proxy :: Proxy (Either Int Int))
            makeMsgPackProp "Either Int (Either Int Int)"
                (Proxy :: Proxy (Either Int (Either Int Int)))
            makeMsgPackProp "Either (Either Int Int) Int"
                (Proxy :: Proxy (Either (Either Int Int) Int))
            makeMsgPackProp "Coin" (Proxy :: Proxy C.Coin)
            makeMsgPackProp "Signature" (Proxy :: Proxy (C.Signature Int))
            makeMsgPackProp "Address" (Proxy :: Proxy C.Address)
            makeMsgPackProp "Mintette" (Proxy :: Proxy C.Mintette)
            makeMsgPackProp "Hash" (Proxy :: Proxy (C.Hash Int))
            makeMsgPackProp "Explorer" (Proxy :: Proxy C.Explorer)
            {-makeMsgPackProp "NewPeriodData" (Proxy :: Proxy C.NewPeriodData)-}
            makeMsgPackProp "SmallNewPeriodData"
                (Proxy :: Proxy SmallNewPeriodData)
            {-makeMsgPackProp "LBlock" (Proxy :: Proxy C.LBlock)-}
            makeMsgPackProp "SmallLBlock" (Proxy :: Proxy SmallLBlock)
            {-makeMsgPackProp "Transaction" (Proxy :: Proxy C.Transaction)-}
            makeMsgPackProp "SmallTransaction" (Proxy :: Proxy SmallTransaction)
            makeMsgPackProp "CheckConfirmation"
                (Proxy :: Proxy C.CheckConfirmation)
            makeMsgPackProp "CommitAcknowledgment"
                (Proxy :: Proxy C.CommitAcknowledgment)
            {-makeMsgPackProp "HBlock" (Proxy :: Proxy C.HBlock)-}
            makeMsgPackProp "SmallHBlock" (Proxy :: Proxy SmallHBlock)
            makeMsgPackProp "TxStrategy" (Proxy :: Proxy C.TxStrategy)
            makeMsgPackProp "PartyAddress" (Proxy :: Proxy C.PartyAddress)
            makeMsgPackProp "AllocationAddress"
                (Proxy :: Proxy C.AllocationAddress)
            makeMsgPackProp "AllocationStrategy"
                (Proxy :: Proxy C.AllocationStrategy)
            makeMsgPackProp "AllocationInfo" (Proxy :: Proxy C.AllocationInfo)
            makeMsgPackProp "Set" (Proxy :: Proxy (S.Set Int))
            makeMsgPackProp "ActionLogEntry" (Proxy :: Proxy C.ActionLogEntry)
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
    err = error "[MessagePackSpec] : Failed MessagePack unpacking!"
