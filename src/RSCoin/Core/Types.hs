{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | More complex types from the paper.

module RSCoin.Core.Types
       ( PeriodId
       , Mintette (..)
       , Mintettes
       , MintetteId
       , Explorer (..)
       , Explorers
       , ActionLogHead
       , ActionLogHeads
       , CheckConfirmation (..)
       , CheckConfirmations
       , CommitAcknowledgment (..)
       , ActionLogEntry (..)
       , ActionLogEntryHash (..)
       , ActionLog
       , LBlock (..)
       , LBlockHash (..)
       , PeriodResult
       , Dpk
       , Utxo
       , Pset
       , HBlock (..)
       , HBlockHash (..)
       , NewPeriodData (..)
       , WithMetadata (..)
       , formatNewPeriodData
       ) where

import           Control.Arrow          (first)
import           Data.Binary            (Binary (get, put), Get, Put)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust, isJust)
import           Data.MessagePack       (MessagePack)
import           Data.SafeCopy          (base, deriveSafeCopy)
import qualified Data.Text.Buildable    as B (Buildable (build))
import           Data.Text.Lazy.Builder (Builder)
import           Data.Word              (Word8)
import           Formatting             (bprint, int, string, (%), build, builder)
import qualified Formatting

import           Serokell.Util.Text     (listBuilderJSON, listBuilderJSONIndent,
                                         mapBuilder, pairBuilder, tripleBuilder)

import           RSCoin.Core.Crypto     (Hash, PublicKey, Signature)
import           RSCoin.Core.Primitives (AddrId, Address, Transaction)
import           RSCoin.Core.Strategy   (AddressToTxStrategyMap)

-- | Periods are indexed by sequence of numbers starting from 0.
type PeriodId = Int

-- | All the information about a particular mintette.
data Mintette = Mintette
    { mintetteHost :: !String
    , mintettePort :: !Int
    } deriving (Show, Eq, Ord)

instance Binary Mintette where
    put Mintette {..} = do
        put mintetteHost
        put mintettePort
    get = Mintette <$> get <*> get

$(deriveSafeCopy 0 'base ''Mintette)

instance B.Buildable Mintette where
    build Mintette{..} = bprint template mintetteHost mintettePort
      where
        template = "Mintette (" % build % ":" % build % ")"

-- | Mintettes list is stored by Bank and doesn't change over period.
type Mintettes = [Mintette]

instance B.Buildable Mintettes where
    build = listBuilderJSON

-- | Mintette is identified by it's index in mintettes list stored by Bank.
-- This id doesn't change over period, but may change between periods.
type MintetteId = Int

-- | All the information about a particular block explorer.
data Explorer = Explorer
    { explorerHost :: !String
    , explorerPort :: !Int
    , explorerKey  :: !PublicKey
    } deriving (Show,Eq,Ord)

instance Binary Explorer where
    put Explorer {..} = do
        put explorerHost
        put explorerPort
        put explorerKey
    get = Explorer <$> get <*> get <*> get

$(deriveSafeCopy 0 'base ''Explorer)

instance B.Buildable Explorer where
    build Explorer{..} =
        bprint
            ("Explorer (" % string % ":" % int % ", pk: " % Formatting.build %
             ")")
            explorerHost
            explorerPort
            explorerKey

-- | List of explorers is stored by bank.
type Explorers = [Explorer]

-- | Each mintette has a log of actions along with hash which is chained.
-- Head of this log is represented by pair of hash and sequence number.
type ActionLogHead = (ActionLogEntryHash, Int)

instance B.Buildable ActionLogHead where
    build = pairBuilder

-- | ActionLogHeads is a map containing head for each mintette with whom
-- the particular mintette has indirectly interacted.
type ActionLogHeads = M.Map PublicKey ActionLogHead

type SignatureActLog = Signature (Transaction, AddrId, ActionLogHead, PeriodId)

-- | CheckConfirmation is a confirmation received by user from mintette as
-- a result of CheckNotDoubleSpent action.
data CheckConfirmation = CheckConfirmation
    { ccMintetteKey       :: !PublicKey       -- ^ key of corresponding mintette
    , ccMintetteSignature :: !SignatureActLog -- ^ signature for (tx, addrid, head)
    , ccHead              :: !ActionLogHead   -- ^ head of log
    , ccPeriodId          :: !PeriodId        -- ^ period id when confirmation was made
    } deriving (Show, Eq, Ord)

instance Binary CheckConfirmation where
    put CheckConfirmation{..} = do
        put ccMintetteKey
        put ccMintetteSignature
        put ccHead
        put ccPeriodId
    get = CheckConfirmation <$> get <*> get <*> get <*> get

instance B.Buildable CheckConfirmation where
    build CheckConfirmation{..} =
        bprint template ccMintetteKey ccMintetteSignature ccHead
      where
        template = "CheckConfirmation (key = " % build %
                   ", sugnature = " % build %
                   ", head = " % build % ")"

-- | CheckConfirmations is a bundle of evidence collected by user and
-- sent to mintette as payload for Commit action.
type CheckConfirmations = M.Map (MintetteId, AddrId) CheckConfirmation

instance B.Buildable CheckConfirmations where
    build = mapBuilder . map (first pairBuilder) . M.assocs

type SignatureActHead = Signature (Transaction, ActionLogHead)

-- | CommitAcknowledgment is sent by mintette to user as an evidence
-- that mintette has included it into lower-level block.
data CommitAcknowledgment = CommitAcknowledgment
    { caMintetteKey       :: !PublicKey        -- ^ key of corresponding mintette
    , caMintetteSignature :: !SignatureActHead -- ^ signature for (tx, logHead)
    , caHead              :: !ActionLogHead    -- ^ head of log
    } deriving (Show, Eq)

instance Binary CommitAcknowledgment where
    put CommitAcknowledgment{..} = do
        put caMintetteKey
        put caMintetteSignature
        put caHead
    get = CommitAcknowledgment <$> get <*> get <*> get

-- | Each mintette mantains a high-integrity action log, consisting of entries.
data ActionLogEntry
    = QueryEntry !Transaction
    | CommitEntry !Transaction
                  !CheckConfirmations
    | CloseEpochEntry !ActionLogHeads
    deriving (Show, Eq)

-- | ActionLogPairs are stored in ActionLog. This pair constists of
-- action log entry and hash of previous pair in log.
type ActionLogPair = (ActionLogEntry, ActionLogEntryHash)

-- | ActionLogEntryHash is a hash of pervious ActionLogPair in log.
newtype ActionLogEntryHash = ALEHash
    { getALEHash :: Hash ActionLogPair
    } deriving (Show, Eq, Ord, Binary, B.Buildable, MessagePack)

putByte :: Word8 -> Put
putByte = put

instance Binary ActionLogEntry where
    put (QueryEntry tr)         = putByte 0 >> put tr
    put (CommitEntry tr cc)     = putByte 1 >> put (tr, cc)
    put (CloseEpochEntry heads) = putByte 2 >> put heads
    get = do
        t <- get :: Get Word8
        case t of
            0 -> QueryEntry <$> get
            1 -> uncurry CommitEntry <$> get
            2 -> CloseEpochEntry <$> get
            _ -> fail "Unexpected ActionLogEntry type"

instance B.Buildable ActionLogEntry where
    build (QueryEntry tx) = bprint ("Query (" % build % ")") tx
    build (CommitEntry tx cc) =
        bprint templateCommit tx cc
      where
        templateCommit = "Commit (tx = " % build %
                         ", confirmations = " % build % ")"
    build (CloseEpochEntry heads) =
        bprint templateClose $ mapBuilder $ M.assocs heads
      where
        templateClose = "CloseEpoch (heads = " % builder % ")"

-- | Action log is a list of entries.
type ActionLog = [(ActionLogEntry, ActionLogEntryHash)]

instance B.Buildable ActionLog where
    build = listBuilderJSONIndent 2 . map pairBuilder

type LBlockInfo = (HBlockHash, ActionLogEntryHash, ActionLogHeads, [Transaction])

newtype LBlockHash = LBlockHash
    { getLBlockHash :: Hash LBlockInfo
    } deriving (Show, Eq, Binary, B.Buildable, MessagePack)

type SignatureLBlock = Signature LBlockHash

-- | Lower-level block generated by mintette in the end of an epoch.
-- To form a lower-level block a mintette uses the transaction set it
-- formed throughout the epoch and the hashes it has received from other
-- mintettes.
data LBlock = LBlock
    { lbHash         :: !LBlockHash      -- ^ hash of
                                         -- (h^(i-1)_bank, h^m_(j-1), hashes, transactions)
    , lbTransactions :: ![Transaction]   -- ^ txset
    , lbSignature    :: !SignatureLBlock -- ^ signature given by mintette for hash
    , lbHeads        :: !ActionLogHeads  -- ^ heads received from other mintettes
    } deriving (Show, Eq)

instance B.Buildable LBlock where
    build LBlock{..} =
        bprint
            template
            lbHash
            (listBuilderJSON lbTransactions)
            lbSignature
            (mapBuilder $ M.assocs lbHeads)
      where
        template =
             "LBlock {\n" %
             "  hash: " % build % "\n" %
             "  transactions: " % builder % "\n"%
             "  signature: " % build % "\n"%
             "  heads: " % builder % "\n"%
             "}\n"

-- | PeriodResult is sent by mintette to bank when period finishes.
type PeriodResult = (PeriodId, [LBlock], ActionLog)

-- | DPK is a list of signatures which authorizies mintettes for one period
type Dpk = [(PublicKey, Signature PublicKey)]

-- | Utxo is a type used by mintettes. (addrid -> addr) ∈ utxo means
-- that there was an act of money transfer to address, but since then
-- it wasn't used.
type Utxo = M.Map AddrId Address

-- | Pset is a type used by mintettes. (addrid -> transaction) ∈ pset
-- means that mintette confirmed this transaction isn't double-spent
-- for the given period.
type Pset = M.Map AddrId Transaction

instance B.Buildable Dpk where
    build = listBuilderJSON . map pairBuilder

newtype HBlockHash = HBlockHash
    { getHBlockHash :: Hash (HBlockHash, [Transaction])
    } deriving (Show,Eq,Binary,B.Buildable,MessagePack)

type SignatureHBlock = Signature HBlockHash

-- | Higher-level block generated by bank in the end of a period.
-- To form a higher-level block bank uses lower-level blocks received
-- from mintettes and simply merges them after checking validity.
data HBlock = HBlock
    { hbHash         :: !HBlockHash
    , hbTransactions :: ![Transaction]
    , hbSignature    :: !SignatureHBlock
    , hbDpk          :: !Dpk
    , hbAddresses    :: !AddressToTxStrategyMap
    } deriving (Show, Eq)

instance Binary HBlock where
    put HBlock{..} = do
        put hbHash
        put hbTransactions
        put hbSignature
        put hbDpk
        put hbAddresses
    get = HBlock <$> get <*> get <*> get <*> get <*> get

instance B.Buildable HBlock where
    build HBlock{..} =
        bprint
            template
            hbHash
            (listBuilderJSON hbTransactions)
            hbSignature
            hbDpk
            (listBuilderJSON hbAddresses)
      where
        template =
            "Block {\n"%
            "  hash: " % build % "\n"%
            "  transactions: " % build % "\n"%
            "  signature: " % build % "\n"%
            "  dpk: " % build % "\n"%
            "  addresses: " % builder % "\n"%
            "}\n"

instance B.Buildable [HBlock] where
  build = listBuilderJSON

type NewMintetteIdPayload = (MintetteId, Utxo, AddressToTxStrategyMap)

-- | Data sent by server on new period start. If mintette id changes,
-- bank *must* include npdNewIdPayload.
data NewPeriodData = NewPeriodData
    { npdPeriodId     :: !PeriodId                     -- ^ Id of a new period
    , npdMintettes    :: !Mintettes                    -- ^ Mintettes list
    , npdHBlock       :: !HBlock                       -- ^ Last processed HBlock (needed to
                                                       -- update local mintette's utxo)
    , npdNewIdPayload :: !(Maybe NewMintetteIdPayload) -- ^ Data needed for mintette to
                                                       -- restore state if it's Id changes
    , npdDpk          :: !Dpk                          -- ^ Dpk
    } deriving (Show, Eq)

instance B.Buildable (AddrId, Address) where
    build = pairBuilder

instance B.Buildable (AddrId, Transaction) where
    build = pairBuilder

instance B.Buildable Utxo where
    build mapping = listBuilderJSON $ M.toList mapping

instance B.Buildable Pset where
    build mapping = listBuilderJSON $ M.toList mapping

instance B.Buildable (Address, Signature a) where
    build = pairBuilder

instance B.Buildable [(Address, Signature a)] where
    build = listBuilderJSONIndent 2

instance B.Buildable [NewPeriodData] where
    build = listBuilderJSONIndent 2

instance B.Buildable AddressToTxStrategyMap where
    build = mapBuilder . M.assocs

instance B.Buildable NewMintetteIdPayload where
    build = tripleBuilder

formatNewPeriodData :: Bool -> NewPeriodData -> Builder
formatNewPeriodData withPayload NewPeriodData{..}
  | withPayload && isJust npdNewIdPayload =
      bprint templateWithPayload
          npdPeriodId npdMintettes (fromJust npdNewIdPayload) npdHBlock
  | otherwise =
      bprint templateWithoutPayload npdPeriodId npdMintettes npdHBlock
  where
    templateWithPayload =
        "NewPeriodData {\n"%
        "  periodId: " % build % "\n"%
        "  mintettes: " % build % "\n"%
        "  newIdPayload: " % build % "\n"%
        "  HBlock: " % build % "\n"%
        "}\n"
    templateWithoutPayload =
        "NewPeriodData {\n" %
        "  periodId: " % build % "\n" %
        "  mintettes: " % build % "\n" %
        "  HBlock: " % build % "\n" %
        "}\n"

instance B.Buildable NewPeriodData where
    build = formatNewPeriodData True

data WithMetadata value metadata = WithMetadata
    { wmValue    :: value
    , wmMetadata :: metadata
    }

instance (B.Buildable value, B.Buildable metadata) =>
         B.Buildable (WithMetadata value metadata) where
    build WithMetadata{..} =
        bprint
            ("WithMetadata:\n— value: " % Formatting.build % "\n— metadata: " %
             Formatting.build)
            wmValue
            wmMetadata

$(deriveSafeCopy 0 'base ''CheckConfirmation)
$(deriveSafeCopy 0 'base ''CommitAcknowledgment)
$(deriveSafeCopy 0 'base ''ActionLogEntryHash)
$(deriveSafeCopy 0 'base ''ActionLogEntry)
$(deriveSafeCopy 0 'base ''LBlockHash)
$(deriveSafeCopy 0 'base ''LBlock)
$(deriveSafeCopy 0 'base ''HBlockHash)
$(deriveSafeCopy 0 'base ''HBlock)
$(deriveSafeCopy 0 'base ''NewPeriodData)
$(deriveSafeCopy 0 'base ''WithMetadata)
