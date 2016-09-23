{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for core datatypes
module RSCoin.Core.Arbitrary
       ( SmallActionLogEntry
       , SmallAllocationInfo
       , SmallAllocationStrategy
       , SmallHBlock
       , SmallLBlock
       , SmallNewPeriodData
       , SmallPeriodResult
       , SmallTransaction
       , SmallTxStrategy
       ) where

import           Data.Binary               (Binary)
import           Data.List                 ()
import qualified Data.Map                  as M
import           Data.MessagePack          (MessagePack (..))
import           Data.SafeCopy             (base, deriveSafeCopy)
import           Test.QuickCheck           (Arbitrary (arbitrary), Gen,
                                            Positive (..), choose, oneof, scale)
import           Test.QuickCheck.Instances ()

import qualified RSCoin.Core.Crypto        as C
import qualified RSCoin.Core.Primitives    as C
import qualified RSCoin.Core.Protocol      as C
import qualified RSCoin.Core.Strategy      as C
import qualified RSCoin.Core.Types         as C

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


instance Arbitrary C.CoinAmount where
    arbitrary = C.CoinAmount . abs <$> arbitrary

instance Arbitrary C.Color where
    arbitrary = C.Color . abs <$> arbitrary

instance Arbitrary C.Coin where
    arbitrary = do
        col <- arbitrary
        coin <- getPositive <$> arbitrary
        return $ C.Coin col coin

instance Arbitrary C.Mintette where
    arbitrary = C.Mintette <$> arbitrary <*> arbitrary

instance Arbitrary C.Explorer where
    arbitrary = C.Explorer <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (C.Hash a) where
    arbitrary = C.unsafeHash <$> (arbitrary :: Gen Int)

instance Arbitrary C.ActionLogEntryHash where
    arbitrary = C.ALEHash <$> arbitrary

instance Arbitrary C.LBlockHash where
    arbitrary = C.LBlockHash <$> arbitrary

instance Arbitrary C.HBlockHash where
    arbitrary = C.HBlockHash <$> arbitrary

instance Arbitrary C.Address where
    arbitrary = C.Address <$> arbitrary

instance Arbitrary C.Transaction where
    arbitrary = C.Transaction <$> arbitrary <*> arbitrary

newtype SmallTransaction =
    SmallTransaction C.Transaction
    deriving (Binary,MessagePack,Show,Eq)

instance Arbitrary SmallTransaction where
    arbitrary = SmallTransaction <$> makeSmall arbitrary

instance Arbitrary C.LBlock where
    arbitrary =
        C.LBlock <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

newtype SmallLBlock =
    SmallLBlock C.LBlock
    deriving (MessagePack,Show,Eq)

instance Arbitrary SmallLBlock where
    arbitrary = SmallLBlock <$> makeSmall arbitrary

instance Arbitrary C.HBlock where
    arbitrary =
        C.HBlock <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
        pure M.empty

newtype SmallHBlock =
    SmallHBlock C.HBlock
    deriving (Binary,MessagePack,Show,Eq)

instance Arbitrary SmallHBlock where
    arbitrary = SmallHBlock <$> makeSmall arbitrary

instance Arbitrary C.CheckConfirmation where
    arbitrary =
        C.CheckConfirmation
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (abs <$> arbitrary)

instance Arbitrary C.CommitAcknowledgment where
    arbitrary =
        C.CommitAcknowledgment
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance (Binary a, Arbitrary a) =>
         Arbitrary (C.Signature a) where
    arbitrary = C.sign <$> arbitrary <*> arbitrary

instance Arbitrary C.TxStrategy where
    arbitrary = oneof [ pure C.DefaultStrategy
                      , uncurry C.MOfNStrategy <$> gen']
      where gen' = do ls <- arbitrary
                      flip (,) ls <$> choose (1, length ls)

newtype SmallTxStrategy =
    SmallTxStrategy C.TxStrategy
    deriving (MessagePack,Binary,Show,Eq)

instance Arbitrary SmallTxStrategy where
    arbitrary = SmallTxStrategy <$> makeSmall arbitrary

instance Arbitrary C.PeriodResult where
    arbitrary =
        C.PeriodResult
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

newtype SmallPeriodResult =
    SmallPeriodResult C.PeriodResult
    deriving (Show,Eq)

instance Arbitrary SmallPeriodResult where
    arbitrary = SmallPeriodResult <$> makeSmall arbitrary

instance Arbitrary C.NewPeriodData where
    arbitrary =
        C.NewPeriodData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

newtype SmallNewPeriodData =
    SmallNewPeriodData C.NewPeriodData
    deriving (Binary,MessagePack,Show,Eq)

instance Arbitrary SmallNewPeriodData where
    arbitrary = SmallNewPeriodData <$> makeSmall arbitrary

instance Arbitrary C.AllocationAddress where
    arbitrary = oneof [ C.TrustAlloc <$> arbitrary
                      , C.UserAlloc <$> arbitrary
                      ]

instance Arbitrary C.PartyAddress where
    arbitrary = oneof [ C.TrustParty <$> arbitrary <*> arbitrary
                      , C.UserParty <$> arbitrary
                      ]

instance Arbitrary C.AllocationStrategy where
    arbitrary = C.AllocationStrategy <$> arbitrary <*> arbitrary

newtype SmallAllocationStrategy =
    SmallAllocationStrategy C.AllocationStrategy
    deriving (MessagePack,Binary,Show, Eq)

instance Arbitrary SmallAllocationStrategy where
    arbitrary = SmallAllocationStrategy <$> makeSmall arbitrary

instance Arbitrary C.AllocationInfo where
    arbitrary = C.AllocationInfo <$> arbitrary <*> arbitrary

newtype SmallAllocationInfo =
    SmallAllocationInfo C.AllocationInfo
    deriving (MessagePack,Show, Eq)

instance Arbitrary SmallAllocationInfo where
    arbitrary = SmallAllocationInfo <$> makeSmall arbitrary

instance Arbitrary C.ActionLogEntry where
    arbitrary = oneof [ C.QueryEntry <$> arbitrary
                      , C.CommitEntry <$> arbitrary <*> arbitrary
                      , C.CloseEpochEntry <$> arbitrary
                      ]

newtype SmallActionLogEntry =
    SmallActionLogEntry C.ActionLogEntry
    deriving (Binary,MessagePack,Show,Eq)

instance Arbitrary SmallActionLogEntry where
    arbitrary = SmallActionLogEntry <$> makeSmall arbitrary

instance Arbitrary C.BankLocalControlRequest where
    arbitrary = oneof [ C.AddMintette <$> arbitrary <*> arbitrary <*> arbitrary
                      , C.AddExplorer <$> arbitrary <*> arbitrary <*> arbitrary
                      , C.RemoveMintette <$> arbitrary <*> arbitrary <*> arbitrary
                      , C.RemoveExplorer <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

instance Arbitrary C.HBlockMetadata where
    arbitrary = C.HBlockMetadata <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (C.WithMetadata a b) where
    arbitrary = C.WithMetadata <$> arbitrary <*> arbitrary

instance (Arbitrary a, Binary a) =>
         Arbitrary (C.WithSignature a) where
    arbitrary = C.mkWithSignature <$> arbitrary <*> arbitrary



$(deriveSafeCopy 0 'base ''SmallActionLogEntry)
$(deriveSafeCopy 0 'base ''SmallAllocationInfo)
$(deriveSafeCopy 0 'base ''SmallLBlock)
$(deriveSafeCopy 0 'base ''SmallHBlock)
$(deriveSafeCopy 0 'base ''SmallNewPeriodData)
$(deriveSafeCopy 0 'base ''SmallTransaction)
$(deriveSafeCopy 0 'base ''SmallTxStrategy)
$(deriveSafeCopy 0 'base ''SmallPeriodResult)
