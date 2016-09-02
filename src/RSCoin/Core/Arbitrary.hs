-- | Arbitrary instances for core datatypes
module RSCoin.Core.Arbitrary () where

import           Data.Binary               (Binary)
import           Data.List                 ()
import qualified Data.Map                  as M
import           Test.QuickCheck           (Arbitrary (arbitrary), Gen,
                                            Positive (..), choose, oneof)
import           Test.QuickCheck.Instances ()

import qualified RSCoin.Core.Crypto        as C
import qualified RSCoin.Core.Primitives    as C
import qualified RSCoin.Core.Protocol      as C
import qualified RSCoin.Core.Strategy      as C
import qualified RSCoin.Core.Types         as C

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

instance Arbitrary C.LBlock where
    arbitrary =
        C.LBlock <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary C.HBlock where
    arbitrary =
        C.HBlock <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
        pure M.empty

instance Arbitrary C.CheckConfirmation where
    arbitrary =
        C.CheckConfirmation <$> arbitrary <*> arbitrary <*> arbitrary <*>
        (abs <$> arbitrary)

instance Arbitrary C.CommitAcknowledgment where
    arbitrary = C.CommitAcknowledgment <$> arbitrary <*> arbitrary <*> arbitrary

instance (Binary a, Arbitrary a) =>
         Arbitrary (C.Signature a) where
    arbitrary = C.sign <$> arbitrary <*> arbitrary

instance Arbitrary C.TxStrategy where
    arbitrary = oneof [ pure C.DefaultStrategy
                      , uncurry C.MOfNStrategy <$> gen']
      where gen' = do ls <- arbitrary
                      flip (,) ls <$> choose (1, length ls)

instance Arbitrary C.NewPeriodData where
    arbitrary =
        C.NewPeriodData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

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

instance Arbitrary C.AllocationInfo where
    arbitrary = C.AllocationInfo <$> arbitrary <*> arbitrary

instance Arbitrary C.ActionLogEntry where
    arbitrary = oneof [ C.QueryEntry <$> arbitrary
                      , C.CommitEntry <$> arbitrary <*> arbitrary
                      , C.CloseEpochEntry <$> arbitrary
                      ]

{-instance Arbitrary [(C.Color, C.Coin)] where
    arbitrary = do
        list <- arbitrary :: Gen [(C.Color, NonNegative Rational)]
        return $ map (\(col,NonNegative rt) -> (col, C.Coin col rt)) list

instance Arbitrary C.CoinsMap where
    arbitrary = do
        list <- arbitrary
        return $ M.fromListWith (+) list

--this instance isn't working at the moment, causes an error.
-}


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
