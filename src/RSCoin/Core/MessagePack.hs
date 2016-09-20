-- | MessagePack serialization/deserialization for Core types

module RSCoin.Core.MessagePack  () where

import           Control.Lens               (view, _3)
import           Data.Bifunctor             (bimap)
import           Data.Binary                (decodeOrFail, encode)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Fixed                 as Fixed
import           Data.Hashable              (Hashable)
import qualified Data.HashSet               as HS
import           Data.Int                   (Int64)
import           Data.MessagePack           (MessagePack (fromObject, toObject), Object (ObjectBin, ObjectExt, ObjectInt),
                                             pack, unpack)
import           Data.Ratio                 (Ratio, denominator, numerator, (%))
import qualified Data.Set                   as S
import           Data.Time.Clock            (NominalDiffTime)
import           Data.Tuple.Curry           (uncurryN)

import           RSCoin.Core.Crypto         ()
import qualified RSCoin.Core.Primitives     as C
import qualified RSCoin.Core.Protocol.Types as C
import qualified RSCoin.Core.Strategy       as C
import qualified RSCoin.Core.Types          as C

toInt :: Integral a => a -> Int
toInt = fromIntegral

fromInt :: Num a => Int -> a
fromInt = fromIntegral

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 = uncurryN

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 = uncurryN

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 = uncurryN

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 = uncurryN

instance MessagePack (Fixed.Fixed a) where
    toObject (Fixed.MkFixed a) = toObject a
    fromObject = fmap Fixed.MkFixed . fromObject

instance MessagePack NominalDiffTime where
    toObject = toObject . (realToFrac :: NominalDiffTime -> Fixed.Pico)
    fromObject = fmap (realToFrac :: Fixed.Pico -> NominalDiffTime) . fromObject

-- msgpack library we use is awful :(
-- RЕАЛLY IT"S S0 AWFUЛ
instance MessagePack Int64 where
    toObject = toObject . toInt
    fromObject = fmap fromInt . fromObject

instance MessagePack Integer where
    toObject i
        | fromInt minBound <= i && i <= fromInt maxBound = ObjectInt $ toInt i
        | otherwise = ObjectBin . BSL.toStrict $ encode i
    fromObject (ObjectInt i) = Just $ fromInt i
    fromObject (ObjectBin b) =
        either (const Nothing) (Just . view _3) . decodeOrFail $ BSL.fromStrict b
    fromObject _             = Nothing

instance MessagePack Word where
    toObject = (toObject :: Integer -> Object) . fromIntegral
    fromObject = fmap (fromIntegral :: Integer -> Word) . fromObject

instance (Integral a, MessagePack a) => MessagePack (Ratio a) where
    toObject r = toObject (numerator r, denominator r)
    fromObject = fmap (uncurry (%)) . fromObject

instance (MessagePack a, MessagePack b) => MessagePack (Either a b) where
    toObject (Left a)  = ObjectExt 0 $ BSL.toStrict $ pack a
    toObject (Right b) = ObjectExt 1 $ BSL.toStrict $ pack b
    fromObject (ObjectExt 0 a) = Left <$> unpack (BSL.fromStrict a)
    fromObject (ObjectExt 1 b) = Right <$> unpack (BSL.fromStrict b)
    fromObject _               = Nothing

instance MessagePack C.Coin where
    toObject (C.Coin c t) = toObject (C.getColor c, C.getAmount t)
    fromObject = fmap (uncurry C.Coin . bimap C.Color C.CoinAmount) . fromObject

instance MessagePack C.Address where
    toObject (C.Address c) = toObject c
    fromObject = fmap C.Address . fromObject

instance MessagePack C.Mintette where
    toObject C.Mintette{..} =
        toObject (toObject mintetteHost, toObject mintettePort)
    fromObject = fmap (uncurry2 C.Mintette) . fromObject

instance MessagePack C.Explorer where
    toObject C.Explorer{..} =
        toObject
            (toObject explorerHost, toObject explorerPort, toObject explorerKey)
    fromObject = fmap (uncurry3 C.Explorer) . fromObject

instance MessagePack C.PeriodResult where
    toObject C.PeriodResult{..} =
        toObject
            (prPeriodId, prBlocks, prActionLog, prBlocksNumber, prActionLogSize)
    fromObject = fmap (uncurry5 C.PeriodResult) . fromObject

instance MessagePack C.NewPeriodData where
    toObject C.NewPeriodData{..} =
        toObject
            (npdPeriodId, npdMintettes, npdHBlock, npdNewIdPayload, npdDpk)
    fromObject = fmap (uncurry5 C.NewPeriodData) . fromObject

instance MessagePack C.LBlock where
    toObject C.LBlock{..} =
        toObject (lbHash, lbTransactions, lbSignature, lbHeads)
    fromObject = fmap (uncurry4 C.LBlock) . fromObject

instance MessagePack C.Transaction where
    toObject C.Transaction{..} = toObject (txInputs, txOutputs)
    fromObject = fmap (uncurry2 C.Transaction) . fromObject

instance MessagePack C.CheckConfirmation where
    toObject C.CheckConfirmation{..} =
        toObject (ccMintetteKey, ccMintetteSignature, ccHead, ccPeriodId)
    fromObject = fmap (uncurry4 C.CheckConfirmation) . fromObject

instance MessagePack C.CommitAcknowledgment where
    toObject C.CommitAcknowledgment{..} =
        toObject (caMintetteKey, caMintetteSignature, caHead)
    fromObject = fmap (uncurry3 C.CommitAcknowledgment) . fromObject

instance MessagePack C.HBlock where
    toObject C.HBlock {..} =
        toObject (hbHash, hbTransactions, hbSignature, hbDpk, hbAddresses)
    fromObject = fmap (uncurry5 C.HBlock) . fromObject

instance MessagePack C.TxStrategy where
    toObject C.DefaultStrategy        = toObj (0, ())
    toObject (C.MOfNStrategy m addrs) = toObj (1, (m, addrs))

    fromObject obj = do
        (i, args) <- fromObject obj
        case (i :: Int) of
            0 -> pure C.DefaultStrategy
            1 -> uncurry2 C.MOfNStrategy <$> fromObject args
            _ -> Nothing

instance MessagePack C.AllocationAddress where
    toObject (C.TrustAlloc addr) = toObj (0, addr)
    toObject (C.UserAlloc  addr) = toObj (1, addr)

    fromObject obj = do
        (i, addr) <- fromObject obj
        case (i :: Int) of
            0 -> C.TrustAlloc <$> fromObject addr
            1 -> C.UserAlloc  <$> fromObject addr
            _ -> Nothing

instance MessagePack C.PartyAddress where
    toObject (C.TrustParty genAddr pubAddr) = toObj (0, (genAddr, pubAddr))
    toObject (C.UserParty  genAddr)         = toObj (1, genAddr)

    fromObject obj = do
        (i, addrs) <- fromObject obj
        case (i :: Int) of
            0 -> uncurry C.TrustParty <$> fromObject addrs
            1 ->         C.UserParty  <$> fromObject addrs
            _ -> Nothing

instance MessagePack C.AllocationStrategy where
    toObject C.AllocationStrategy{..} = toObject (_sigNumber, _allParties)
    fromObject = fmap (uncurry C.AllocationStrategy) . fromObject

instance MessagePack C.AllocationInfo where
    toObject C.AllocationInfo{..} = toObject (_allocationStrategy, _currentConfirmations)
    fromObject = fmap (uncurry C.AllocationInfo) . fromObject

instance (Ord e, MessagePack e) => MessagePack (S.Set e) where
    toObject = toObject . S.toList
    fromObject = fmap S.fromList . fromObject

instance (Eq e, Hashable e, MessagePack e) => MessagePack (HS.HashSet e) where
    toObject = toObject . HS.toList
    fromObject = fmap HS.fromList . fromObject

toObj
    :: MessagePack a
    => (Int, a) -> Object
toObj = toObject

instance MessagePack C.ActionLogEntry where
    toObject (C.QueryEntry tx)         = toObj (0, tx)
    toObject (C.CommitEntry tx cc)     = toObj (1, (tx, cc))
    toObject (C.CloseEpochEntry heads) = toObj (2, heads)
    fromObject obj = do
        (i,payload) <- fromObject obj
        case (i :: Int) of
            0 -> C.QueryEntry <$> fromObject payload
            1 -> uncurry2 C.CommitEntry <$> fromObject payload
            2 -> C.CloseEpochEntry <$> fromObject payload
            _ -> Nothing

instance MessagePack C.BankLocalControlRequest where
    toObject (C.AddMintette m pk sig)         = toObj (0, (m, pk, sig))
    toObject (C.PermitMintette pk sig)        = toObj (1, (pk, sig))
    toObject (C.AddExplorer e pid sig)        = toObj (2, (e, pid, sig))
    toObject (C.RemoveMintette host port sig) = toObj (3, (host, port, sig))
    toObject (C.RemoveExplorer host port sig) = toObj (4, (host, port, sig))
    toObject (C.FinishPeriod sig)             = toObj (5, sig)
    toObject (C.DumpStatistics sId sig)       = toObj (6, (sId, sig))
    fromObject obj = do
        (i,payload) <- fromObject obj
        case (i :: Int) of
            0 -> uncurry3 C.AddMintette <$> fromObject payload
            1 -> uncurry2 C.PermitMintette <$> fromObject payload
            2 -> uncurry3 C.AddExplorer <$> fromObject payload
            3 -> uncurry3 C.RemoveMintette <$> fromObject payload
            4 -> uncurry3 C.RemoveExplorer <$> fromObject payload
            5 -> C.FinishPeriod <$> fromObject payload
            6 -> uncurry2 C.DumpStatistics <$> fromObject payload
            _ -> Nothing

instance MessagePack C.HBlockMetadata where
    toObject C.HBlockMetadata {..} = toObject (hbmTimestamp, hbmEmission)
    fromObject = fmap (uncurry2 C.HBlockMetadata) . fromObject

instance (MessagePack a, MessagePack b) =>
         MessagePack (C.WithMetadata a b) where
    toObject C.WithMetadata{..} = toObject (wmValue, wmMetadata)
    fromObject = fmap (uncurry C.WithMetadata) . fromObject

instance (MessagePack a) =>
         MessagePack (C.WithSignature a) where
    toObject C.WithSignature {..} = toObject (wsValue, wsSignature)
    fromObject = fmap (uncurry C.WithSignature) . fromObject
