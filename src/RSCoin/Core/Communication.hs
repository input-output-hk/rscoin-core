{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | This module provides high-abstraction functions to exchange data
-- within user/mintette/bank.

module RSCoin.Core.Communication
       ( CommunicationError (..)

         -- * Helpers
       , P.unCps
       , guardTransactionValidity

         -- * Call Bank
         -- ** Local control
       , sendBankLocalControlRequest

         -- ** Simple getters
       , blocksQueryLimit
       , getBlockByHeight
       , getBlockchainHeight
       , getBlocksByHeight
       , getExplorers
       , getGenesisBlock
       , getMintettes
       , getStatisticsId
       -- , getAddresses

       -- ** Actionables
       , addMintetteUsingPermission

         -- * Call Mintette
         -- ** Main methods
       , actionLogQueryLimit
       , announceNewPeriod
       , checkNotDoubleSpent
       , checkNotDoubleSpentBatch
       , commitTx
       , getExtraMintetteBlocks
       , getExtraMintetteLogs
       , lBlocksQueryLimit
       , sendPeriodFinished

         -- ** Simple getters
       , getMintetteLogs
       , getMintettePeriod
       , getMintetteUtxo

         -- * Call Notary
       , allocateMultisignatureAddress
       , announceNewPeriodToNotary
       , getNotaryPeriod
       , getTxSignatures
       , pollPendingTransactions
       , pollPendingTransactionsNoLimit
       , pollTransactionsLimit
       , publishTxToNotary
       , queryNotaryCompleteMSAddresses
       , queryNotaryMyMSAllocations
       , removeNotaryCompleteMSAddresses

         -- * Call Explorer
         -- ** Get info from Bank
       , announceNewBlock

         -- ** Serve Users
       , askExplorer
       , getTransactionById
       ) where

import           Control.Exception          (Exception (..))
import           Control.Lens               (view)
import           Control.Monad              (unless, when)
import           Control.Monad.Catch        (MonadThrow (throwM), catch)
import           Control.Monad.Extra        (unlessM)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.Binary                (Binary)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.MessagePack           (MessagePack)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack)
import qualified Data.Text.Buildable        as B (Buildable (build))
import           Data.Typeable              (Typeable)
import           Formatting                 (build, int, sformat, shown, stext,
                                             (%))
import qualified Network.MessagePack.Client as MP (RpcError (..))
import           Safe                       (atMay, headMay)
import           System.Random              (randomRIO)

import           Serokell.Util.Text         (listBuilderJSON,
                                             listBuilderJSONIndent, mapBuilder,
                                             pairBuilder, show')

import           Control.TimeWarp.Timed     (MonadTimed, MonadTimedError (..))

import           RSCoin.Core.Crypto         (PublicKey, SecretKey, Signature,
                                             hash, derivePublicKey)
import           RSCoin.Core.Error          (rscExceptionFromException,
                                             rscExceptionToException)
import           RSCoin.Core.Logging        (WithNamedLogger (..))
import qualified RSCoin.Core.Logging        as L
import           RSCoin.Core.NodeConfig     (WithNodeContext (getNodeContext),
                                             bankPublicKey, isTestRun,
                                             notaryPublicKey)
import           RSCoin.Core.Primitives     (AddrId, Address, Transaction,
                                             TransactionId)
import qualified RSCoin.Core.Protocol       as P
import           RSCoin.Core.Strategy       (AllocationAddress, AllocationInfo,
                                             AllocationStrategy, MSAddress,
                                             PartyAddress, TxStrategy)
import           RSCoin.Core.Transaction    (validateTxPure, TxVerdict(..))
import           RSCoin.Core.Types          (ActionLog, CheckConfirmation,
                                             CheckConfirmations,
                                             CommitAcknowledgment,
                                             Explorer (..), Explorers, HBlock,
                                             HBlockMetadata, LBlock (..),
                                             Mintette, MintetteId, Mintettes,
                                             NewPeriodData, PeriodId,
                                             PeriodResult, Utxo, WithMetadata,
                                             WithSignature (..),
                                             mkWithSignature,
                                             verifyWithSignature)
import           RSCoin.Core.WorkMode       (WorkMode)

-- | Errors which may happen during remote call.
data CommunicationError
    = ProtocolError Text  -- ^ Message was encoded incorrectly.
    | TimeoutError Text   -- ^ Waiting too long for the reply
    | MethodError Text    -- ^ Error occurred during method execution.
    | BadSignature Text   -- ^ Result of method must be signed, but
                          -- signature is bad.
    | BadRequest Text     -- ^ Request is bad for some reason.
    deriving (Show, Typeable)

instance Exception CommunicationError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance B.Buildable CommunicationError where
    build (ProtocolError t) = "internal error: " <> B.build t
    build (TimeoutError t)  = "timeout error: " <> B.build t
    build (MethodError t)   = "method error: " <> B.build t
    build (BadSignature t)  = B.build t <> " has provided a bad signature"
    build (BadRequest t)    = B.build t

rpcErrorHandler :: (MonadIO m, WithNamedLogger m) => MP.RpcError -> m a
rpcErrorHandler = liftIO . log' . fromError
  where
    log' (e :: CommunicationError) = do
        L.logError $ show' e
        throwM e
    fromError (MP.ProtocolError s)   = ProtocolError $ pack s
    fromError (MP.ResultTypeError s) = ProtocolError $ pack s
    fromError (MP.ServerError obj)   = MethodError $ pack $ show obj

monadTimedHandler :: (MonadTimed m, MonadIO m, WithNamedLogger m) => MonadTimedError -> m a
monadTimedHandler = log' . fromError
  where
    log' (e :: CommunicationError) = do
        L.logError $ show' e
        throwM e
    fromError (MTTimeoutError s) = TimeoutError s

handleErrors :: (WorkMode m, MessagePack a) => m a -> m a
handleErrors action = action `catch` rpcErrorHandler `catch` monadTimedHandler

handleEither :: (WorkMode m, MessagePack a) => m (Either Text a) -> m a
handleEither action = do
    res <- action
    either
        (throwM . MethodError . sformat ("Error on caller side has occurred: " % stext))
        return
        res

withResult :: WorkMode m => m () -> (a -> m ()) -> m a -> m a
withResult before after action = do
    before
    a <- action
    a <$ after a

data Signer
    = SignerBank
    | SignerNotary

signerName :: Signer -> Text
signerName SignerBank   = "bank"
signerName SignerNotary = "notary"

signerKey
    :: (Functor m, WithNodeContext m)
    => Signer -> m PublicKey
signerKey SignerBank   = view bankPublicKey <$> getNodeContext
signerKey SignerNotary = view notaryPublicKey <$> getNodeContext

-- Copy-paste is bad, but I hope this code will be rewritten soon anyway.
withSignedResult
    :: (Binary a, WorkMode m)
    => Signer -> m () -> (a -> m ()) -> m (WithSignature a) -> m a
withSignedResult signer before after action = do
    before
    ws@WithSignature {..} <- action
    pk <- signerKey signer
    let pkOwner = signerName signer
    unless (verifyWithSignature pk ws) $ throwM $ BadSignature pkOwner
    wsValue <$ after wsValue

guardTransactionValidity :: MonadThrow m => Transaction -> m ()
guardTransactionValidity tx = case validateTxPure tx of
    TxValid       -> return ()
    TxInvalid err -> throwM $ BadRequest $
      "Your transaction is not valid: " <> err

---- —————————————————————————————————————————————————————————— ----
---- Bank endpoints ——————————————————————————————————————————— ----
---- —————————————————————————————————————————————————————————— ----

callBank :: (WorkMode m, MessagePack a) => P.Client (Either Text a) -> m a
callBank = handleEither . handleErrors . P.callBankSafe

sendBankLocalControlRequest :: WorkMode m => P.BankLocalControlRequest -> m ()
sendBankLocalControlRequest request =
    withResult
        (L.logDebug $ sformat ("Sending control request to bank: " % build) request)
        (const $ L.logDebug "Sent control request successfully") $
         callBank $ P.call (P.RSCBank P.LocalControlRequest) request

-- | Given the height/perioud id, retreives block if it's present
getBlockByHeight :: WorkMode m => PeriodId -> m (Maybe HBlock)
getBlockByHeight pId = headMay <$> getBlocksByHeight pId pId

-- | Retrieves blockchainHeight from the server
getBlockchainHeight :: WorkMode m => m PeriodId
getBlockchainHeight =
    withSignedResult
        SignerBank
        (L.logDebug "Getting blockchain height")
        (L.logDebug . sformat ("Blockchain height is " % int))
        $ callBank $ P.call (P.RSCBank P.GetBlockchainHeight)

-- | Maximum number of higher-level blocks to be queried in a single
-- request.
blocksQueryLimit :: Num a => a
blocksQueryLimit = 20

getBlocksByHeight :: WorkMode m => PeriodId -> PeriodId -> m [HBlock]
getBlocksByHeight from to =
    withSignedResult
        SignerBank
        infoMessage
        successMessage
        $ callBank $ P.call (P.RSCBank P.GetHBlocks) [from..to]
  where
    infoMessage =
        L.logDebug $
            sformat ("Getting higher-level blocks between " % int % " and " % int)
                from to
    successMessage res =
        L.logDebug $
            sformat
                ("Got higher-level blocks between " % int % " " %
                 int % ": " % build)
                from to (listBuilderJSONIndent 2 res)

getExplorers :: WorkMode m => m Explorers
getExplorers =
    withSignedResult
        SignerBank
        (L.logDebug "Getting list of explorers")
        (L.logDebug .
         sformat ("Successfully got list of explorers " % build) .
         listBuilderJSON) $
    callBank $ P.call (P.RSCBank P.GetExplorers)

getGenesisBlock :: WorkMode m => m HBlock
getGenesisBlock =
    fromMaybe (error "genesis block can't be fetched") <$> getBlockByHeight 0

getMintettes :: WorkMode m => m Mintettes
getMintettes =
    withSignedResult
        SignerBank
        (L.logDebug "Getting list of mintettes")
        (L.logDebug . sformat ("Successfully got list of mintettes " % build)) $
    callBank $ P.call (P.RSCBank P.GetMintettes)

getStatisticsId :: WorkMode m => m Int
getStatisticsId =
    withSignedResult
        SignerBank
        (L.logDebug "Getting statistics id")
        (L.logDebug . sformat ("Statistics id is " % int)) $
    callBank $ P.call (P.RSCBank P.GetStatisticsId)

addMintetteUsingPermission
    :: WorkMode m
    => SecretKey
    -> String
    -> Int
    -> m ()
addMintetteUsingPermission mintetteSK host port = do
    L.logDebug $ sformat
        ("Adding mintette " % build % " listening on port " % int)
        host
        port
    let signed = mkWithSignature mintetteSK (host, port)
    let mintettePK = derivePublicKey mintetteSK
    callBank $ P.call (P.RSCBank P.AddMintetteUsingPermit) mintettePK signed

---- —————————————————————————————————————————————————————————— ----
---- Mintette endpoints ——————————————————————————————————————— ----
---- —————————————————————————————————————————————————————————— ----

callMintette :: (WorkMode m, MessagePack a) => Mintette -> P.Client a -> m a
callMintette m = handleErrors . P.callMintetteSafe m

announceNewPeriod
    :: WorkMode m
    => Mintette -> SecretKey -> NewPeriodData -> m ()
announceNewPeriod mintette bankSK npd = do
    L.logDebug $
        sformat
            ("Announce new period to mintette " % build % ", new period data " %
             build)
            mintette
            npd
    let signed = mkWithSignature bankSK npd
    handleEither $
        callMintette mintette $
        P.call (P.RSCMintette P.AnnounceNewPeriod) signed

checkNotDoubleSpent
    :: WorkMode m
    => Mintette
    -> Transaction
    -> AddrId
    -> [(Address, Signature Transaction)]
    -> m (Either Text CheckConfirmation)
checkNotDoubleSpent m tx a s = do
    guardTransactionValidity tx
    withResult infoMessage (either onError onSuccess) $
        callMintette m $ P.call (P.RSCMintette P.CheckTx) tx a s
  where
    infoMessage =
        L.logDebug $ sformat ("Checking addrid (" % build % ") from transaction: " % build) a tx
    onError e =
        L.logError $ sformat ("Checking double spending failed: " % stext) e
    onSuccess res = do
        L.logDebug $
            sformat ("Confirmed addrid (" % build % ") from transaction: " % build) a tx
        L.logDebug $ sformat ("Confirmation: " % build) res

checkNotDoubleSpentBatch
    :: forall m.
       WorkMode m
    => Mintette
    -> Transaction
    -> M.Map AddrId [(Address, Signature Transaction)]
    -> m (M.Map AddrId (Either Text CheckConfirmation))
checkNotDoubleSpentBatch m tx signatures = do
    guardTransactionValidity tx
    withResult infoMessage onReturn $ handleEither $
        callMintette m $ P.call (P.RSCMintette P.CheckTxBatch) tx signatures
  where
    infoMessage =
        L.logDebug $ sformat ("Checking addrids (" % build
                              % ") from transaction: " % build)
                             (listBuilderJSON $ M.keys signatures)
                             tx
    onReturn :: M.Map AddrId (Either Text CheckConfirmation) -> m ()
    onReturn _ =
        L.logDebug $
            sformat ("Confirmed signatures from transaction: " % build) tx
--        L.logDebug $ sformat ("Confirmations: " % build) $
--            listBuilderJSON $ map pairBuilder $ M.assocs res
--      TODO add this log call (something bad with buildable)

commitTx
    :: WorkMode m
    => Mintette
    -> Transaction
    -> CheckConfirmations
    -> m (Either Text CommitAcknowledgment)
commitTx m tx cc = do
    guardTransactionValidity tx
    withResult infoMessage (either onError onSuccess) $
        callMintette m $ P.call (P.RSCMintette P.CommitTx) tx cc
  where
    infoMessage = L.logDebug $ sformat ("Commit transaction " % build) tx
    onError e = L.logError $ sformat ("Commit tx failed: " % stext) e
    onSuccess _ =
        L.logDebug $ sformat ("Successfully committed transaction " % build) tx

sendPeriodFinished
    :: WorkMode m
    => Mintette -> SecretKey -> PeriodId -> m PeriodResult
sendPeriodFinished mintette bankSK pId =
    withResult infoMessage successMessage $
    handleEither $
    callMintette mintette $ P.call (P.RSCMintette P.PeriodFinished) signed
  where
    signed = mkWithSignature bankSK pId
    infoMessage =
        L.logDebug $
        sformat ("Send period " % int % " finished to mintette " % build)
            pId mintette
    successMessage =
        L.logDebug .
        sformat
            ("Received period result from mintette " % build % ": \n" % build)
            mintette

-- | Maximum number of lower-level blocks to be queried from mintette
-- in a single request.
lBlocksQueryLimit :: Num a => a
lBlocksQueryLimit = 10

-- | Maximum number of lower-level blocks to be queried from mintette
-- in a single request.
actionLogQueryLimit :: Num a => a
actionLogQueryLimit = 100

getExtraMintetteBlocks
    :: WorkMode m
    => Mintette -> SecretKey -> PeriodId -> (Word, Word) -> m [LBlock]
getExtraMintetteBlocks mintette bankSK pId range =
    withResult infoMessage checkResultAndLog $
    handleEither $
    callMintette mintette $ P.call (P.RSCMintette P.GetExtraBlocks) signed
  where
    signed = mkWithSignature bankSK (pId, range)
    infoMessage =
        L.logDebug $
        sformat
            ("Requesting extra LBlocks (" % int % ", " % int % ")" % ") from mintette " % build)
            (fst range)
            (snd range)
            mintette
    checkResultAndLog blocks = do
        L.logDebug $
            sformat ("Received extra blocks from mintette " % build) mintette
        mapM_ checkResult blocks
    checkResult block =
        when (null (lbTransactions block)) $
        throwM $ MethodError "mintette sent LBlock without transactions"

getExtraMintetteLogs
    :: WorkMode m
    => Mintette -> SecretKey -> PeriodId -> (Word, Word) -> m ActionLog
getExtraMintetteLogs mintette bankSK pId range =
    withResult infoMessage logResult $
    handleEither $
    callMintette mintette $ P.call (P.RSCMintette P.GetExtraLogs) signed
  where
    signed = mkWithSignature bankSK (pId, range)
    infoMessage =
        L.logDebug $
        sformat
            ("Requesting extra ActionLog (" % int % ", " % int % ") from mintette " % build)
            (fst range)
            (snd range)
            mintette
    logResult _ = do
        L.logDebug $
            sformat ("Received extra logs from mintette " % build) mintette

getMintetteLogs :: WorkMode m => MintetteId -> PeriodId -> m (Maybe ActionLog)
getMintetteLogs mId pId = do
    ms <- getMintettes
    maybe onNothing onJust $ ms `atMay` mId
  where
    onNothing = do
        let e = sformat ("Mintette with index " % int % " doesn't exist") mId
        L.logWarning e
        throwM $ MethodError e
    onJust mintette =
        withResult infoMessage (maybe onError onSuccess) $
        handleEither $
        callMintette mintette $ P.call (P.RSCDump P.GetMintetteLogs) pId
    infoMessage =
        L.logDebug $
        sformat ("Getting logs of mintette " % int % " with period id " % int)
        mId pId
    onError =
        L.logWarning $
        sformat
            ("Getting logs of mintette " % int %
                " with period id " % int % " failed")
            mId pId
    onSuccess res =
        L.logDebug $
        sformat
            ("Successfully got logs for period id " % int % ": " % build)
            pId (listBuilderJSONIndent 2 $ map pairBuilder res)

getMintettePeriod :: WorkMode m => Mintette -> m (Maybe PeriodId)
getMintettePeriod m =
    withResult infoMessage (maybe onError onSuccess) $
    handleEither $ callMintette m $ P.call (P.RSCMintette P.GetMintettePeriod)
  where
    infoMessage = L.logDebug $
        sformat ("Getting minette period from mintette " % build) m
    onError = L.logError $ sformat
        ("getMintettePeriod failed for mintette " % build) m
    onSuccess p =
        L.logDebug $ sformat ("Successfully got the period: " % build) p

getMintetteUtxo :: WorkMode m => MintetteId -> m Utxo
getMintetteUtxo mId = do
    unlessM isTestRun $
        throwM $ BadRequest "getMintetteUtxo is only available in test run"
    ms <- getMintettes
    maybe onNothing onJust $ ms `atMay` mId
  where
    onNothing = liftIO $ do
        let e = sformat ("Mintette with this index " % int % " doesn't exist") mId
        L.logWarning e
        throwM $ MethodError e
    onJust mintette =
        withResult
            (L.logDebug "Getting utxo")
            (L.logDebug . sformat ("Current utxo is: " % build))
            (handleEither $
             callMintette mintette $ P.call (P.RSCDump P.GetMintetteUtxo))

---- —————————————————————————————————————————————————————————— ----
---- Notary endpoints ————————————————————————————————————————— ----
---- —————————————————————————————————————————————————————————— ----

callNotary :: (WorkMode m, MessagePack a) => P.Client (Either Text a) -> m a
callNotary = handleEither . handleErrors . P.callNotary

callNotarySafe :: (WorkMode m, MessagePack a) => P.Client (Either Text a) -> m a
callNotarySafe = handleEither . handleErrors . P.callNotarySafe

allocateMultisignatureAddress
    :: WorkMode m
    => Address
    -> PartyAddress
    -> AllocationStrategy
    -> Signature (MSAddress, AllocationStrategy)
    -> Maybe (PublicKey, Signature PublicKey)
    -> m ()
allocateMultisignatureAddress msAddr partyAddr allocStrat signature mMasterCheck = do
    L.logDebug $ sformat
        ( "Allocate new ms address: " % build % "\n ,"
        % "from party address: "      % build % "\n ,"
        % "allocation strategy: "     % build % "\n ,"
        % "current party pair: "      % build % "\n ,"
        % "certificate chain: "       % build % "\n ,"
        )
        msAddr
        partyAddr
        allocStrat
        signature
        (pairBuilder <$> mMasterCheck)
    callNotarySafe $ P.call (P.RSCNotary P.AllocateMultisig)
        msAddr partyAddr allocStrat signature mMasterCheck

announceNewPeriodToNotary
    :: WorkMode m
    => SecretKey
    -> PeriodId
    -> HBlock
    -> m ()
announceNewPeriodToNotary bankSK pIdLast block = do
    L.logDebug $ sformat
        ("Announce new periods to Notary, hblocks " % build %
         ", latest periodId " % int)
        block
        pIdLast
    let signed = mkWithSignature bankSK (pIdLast, block)
    callNotary $ P.call (P.RSCNotary P.AnnounceNewPeriodsToNotary) signed

getNotaryPeriod :: WorkMode m => m PeriodId
getNotaryPeriod =
    withSignedResult
        SignerNotary
        (L.logDebug "Getting period of Notary")
        (L.logDebug . sformat ("Notary's last period is " % int)) $
    callNotary $ P.call $ P.RSCNotary P.GetNotaryPeriod

-- | Read-only method of Notary. Returns current state of signatures
-- for the given address (that implicitly defines addrids ~
-- transaction inputs) and transaction itself.
getTxSignatures :: WorkMode m => Transaction -> Address -> m [(Address, Signature Transaction)]
getTxSignatures tx addr = do
    guardTransactionValidity tx
    withSignedResult SignerNotary infoMessage successMessage $
        callNotarySafe $ P.call (P.RSCNotary P.GetSignatures) tx addr
  where
    infoMessage =
        L.logDebug $
        sformat ("Getting signatures for tx " % shown
                 % ", hash " % build % ", addr " % shown )
                tx (hash tx) addr
    successMessage res =
        L.logDebug $ sformat ("Received signatures from Notary: " % shown) res

-- | Maximum number of addresses which may be used to poll pending transactions.
pollTransactionsLimit :: Num a => a
pollTransactionsLimit = 10

-- | This method is supposed to be used to detect transactions
-- that you `may` want to sign.
pollPendingTransactions
    :: WorkMode m
    => [Address]
    -> m [Transaction]
pollPendingTransactions parties =
    withSignedResult SignerNotary infoMessage successMessage $
    callNotarySafe $ P.call (P.RSCNotary P.PollPendingTransactions) parties
  where
    infoMessage =
        L.logDebug $
        sformat ("Polling transactions to sign for addresses: " % shown) parties
    successMessage res =
        L.logDebug $ sformat ("Received transactions to sign: " % shown) res

-- | Semantics of this method is the same as of
-- pollPendingTransactions, but it does multiple calls if there are
-- too many addresses (because there is a limit on a single call).
pollPendingTransactionsNoLimit
    :: WorkMode m
    => [Address]
    -> m [Transaction]
pollPendingTransactionsNoLimit parties =
    concat <$> mapM pollChunk [0 .. chunksNumber - 1]
  where
    n = length parties
    chunksNumber = (n - 1) `div` pollTransactionsLimit + 1
    pollChunk chunkIdx =
        pollPendingTransactions $
        map
            (parties !!)
            [chunkIdx * pollTransactionsLimit .. min
                                                     ((chunkIdx + 1) *
                                                      pollTransactionsLimit - 1)
                                                     (n - 1)]

-- | Send transaction with public wallet address & signature for it,
-- get list of signatures after Notary adds yours.
publishTxToNotary
    :: WorkMode m
    => Transaction                          -- ^ transaction to sign
    -> Address                              -- ^ address of transaction input (individual, multisig or etc.)
    -> (Address, Signature Transaction)     -- ^ party's public address and signature
                                -- (made with its secret key)
    -> m [(Address, Signature Transaction)] -- ^ signatures for all parties already signed the transaction
publishTxToNotary tx addr sg = do
    guardTransactionValidity tx
    withSignedResult SignerNotary infoMessage successMessage $
        callNotarySafe $ P.call (P.RSCNotary P.PublishTransaction) tx addr sg
  where
    infoMessage =
        L.logDebug $
        sformat ("Sending tx, signature to Notary: " % shown) (tx, sg)
    successMessage res =
        L.logDebug $ sformat ("Received signatures from Notary: " % shown) res

queryNotaryCompleteMSAddresses :: WorkMode m => m [(Address, TxStrategy)]
queryNotaryCompleteMSAddresses =
    withSignedResult
        SignerNotary
        (L.logDebug "Querying Notary complete MS addresses")
        (const $ pure ()) $
    callNotary $ P.call $ P.RSCNotary P.QueryCompleteMS

queryNotaryMyMSAllocations
    :: WorkMode m
    => AllocationAddress
    -> m [(MSAddress, AllocationInfo)]
queryNotaryMyMSAllocations allocAddr =
    withSignedResult SignerNotary infoMessage successMessage $
    callNotarySafe $ P.call (P.RSCNotary P.QueryMyAllocMS) allocAddr
  where
    infoMessage = L.logDebug "Calling Notary for my MS addresses..."
    successMessage res =
        L.logDebug $
        sformat ("Retrieving from Notary: " % build) $ mapBuilder res

removeNotaryCompleteMSAddresses :: WorkMode m => [Address] -> Signature [Address] -> m ()
removeNotaryCompleteMSAddresses addresses signedAddrs = do
    L.logDebug "Removing Notary complete MS addresses"
    callNotary $ P.call (P.RSCNotary P.RemoveCompleteMS) addresses signedAddrs

---- —————————————————————————————————————————————————————————— ----
---- Explorer endpoints ——————————————————————————————————————— ----
---- —————————————————————————————————————————————————————————— ----

-- callExplorer
--     :: (WorkMode m, MessagePack a)
--     => Explorer -> P.Client (Either Text a) -> m a
-- callExplorer e = handleEither . handleErrors . P.callExplorer e

callExplorerSafe
    :: (WorkMode m, MessagePack a)
    => Explorer -> P.Client (Either Text a) -> m a
callExplorerSafe e = handleEither . handleErrors . P.callExplorerSafe e

announceNewBlock
    :: WorkMode m
    => Explorer
    -> SecretKey
    -> PeriodId
    -> WithMetadata HBlock HBlockMetadata
    -> m PeriodId
announceNewBlock explorer bankSK pId blk =
    withResult infoMessage successMessage $
    callExplorerSafe explorer $
    P.call (P.RSCExplorer P.EMNewBlock) signed
  where
    signed = mkWithSignature bankSK (pId, blk)
    infoMessage =
        L.logDebug $
        sformat
            ("Announcing new (" % int % "-th) block to " % build)
            pId
            explorer
    successMessage respPeriod =
        L.logDebug $
        sformat
            ("Received periodId " % int % " from explorer " % build)
            respPeriod
            explorer

askExplorer :: WorkMode m => (Explorer -> m a) -> m a
askExplorer query = do
    explorers <- getExplorers
    when (null explorers) $
        throwM $ MethodError "There are no active explorers"
-- TODO: ask other explorers in case of error
    query . (explorers !!) =<< liftIO (randomRIO (0, length explorers - 1))

getTransactionById
    :: WorkMode m
    => TransactionId -> Explorer -> m (Maybe Transaction)
getTransactionById tId explorer =
    withResult
        (L.logDebug $ sformat ("Getting transaction by id " % build) tId)
        (\t ->
              L.logDebug $
              sformat
                  ("Successfully got transaction by id " % build % ": " % build)
                  tId
                  t) $
    callExplorerSafe explorer $ P.call (P.RSCExplorer P.EMGetTransaction) tId
