{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Functions related to Transaction

module RSCoin.Core.Transaction
       ( TxVerdict(..)
       , canonizeTx
       , validateTxSize
       , isValidTx
       , validateTxPure
       , validateSignature
       , getAmountByAddress
       , getAddrIdByAddress
       , chooseAddresses
       , computeOutputAddrids
       ) where

import           Control.Arrow          ((&&&))
import           Control.Exception      (assert)
import           Control.Lens           (view, _3)
import           Control.Monad          (when, unless)
import           Data.Foldable          (foldl')
import           Data.Function          (on)
import qualified Data.IntMap.Strict     as M
import           Data.List              (groupBy, sortBy)
import           Data.Monoid            ((<>))
import           Data.Ord               (comparing)
import           Data.Text              (Text)

import           RSCoin.Core.Coin       (coinsToMap)
import           RSCoin.Core.Constants  (maxTxSize)
import           RSCoin.Core.Crypto     (Signature, hash, verify)
import           RSCoin.Core.Primitives (AddrId, Address (..), Coin (..),
                                         Color (..), Transaction (..), grey)

validateTxSize :: Transaction -> Bool
validateTxSize Transaction {..} =
    length txInputs + length txOutputs <= maxTxSize

-- | Validate a transaction.
--
-- See 'validateTxPure' if you want to know why validation fails.
isValidTx :: Transaction -> Bool
isValidTx tx = validateTxPure tx == TxValid

data TxVerdict = TxValid | TxInvalid Text
    deriving (Eq, Show)

-- | Validates that sum of inputs for each color isn't greater than
-- sum of outputs, and what's left can be painted by grey coins.
-- Furthermore, the function also checks if the transaction has empty
-- input and/or output lists, and if any of the coins in either list
-- has a negative or zero value. Another check is transaction size
-- (length of inputs + length of outputs) which can't be more than
-- maxTxSize. Violation of any of these conditions makes the transaction
-- invalid.
validateTxPure :: Transaction -> TxVerdict
validateTxPure tx@Transaction{..} =
    either TxInvalid id $ do
        when (totalOutputs > totalInputs) $
            Left "outputs' sum is bigger than inputs' sum"
        when (greyOutputs + greyNeededForPainting > greyInputs) $
            Left "there's not enough uncolored coins to paint"
        when (null txInputs) $
            Left "at least one input is required"
        when (null txOutputs) $
            Left "at least one output is required"
        when (any ((<= 0) . coinAmount) rawInputCoins) $
            Left "some inputs are zero or negative"
        when (any ((<= 0) . coinAmount) rawOutputCoins) $
            Left "some outputs are zero or negative"
        unless (validateTxSize tx) $
            Left "maximum transaction size is exceeded"
        return TxValid
  where
    rawInputCoins  = map (view _3) txInputs
    rawOutputCoins = map snd txOutputs
    -- Note that there'll be only one input and output of each color,
    -- since coinsToMap groups coins by color
    inputs  = coinsToMap rawInputCoins
    outputs = coinsToMap rawOutputCoins
    colorAmount color list = maybe 0 coinAmount (M.lookup color list)
    totalInputs  = sum $ map coinAmount $ M.elems inputs
    totalOutputs = sum $ map coinAmount $ M.elems outputs
    greyInputs  = colorAmount (getColor grey) inputs
    greyOutputs = colorAmount (getColor grey) outputs
    -- Uncolored coins can become colored, but colored coins can't change
    -- color. E.g. this transaction is invalid:
    --     [Coin (Color 3) 10, Coin (Color 4) 5] ->
    --     [Coin (Color 3) 10, Coin (Color 2) 4]
    -- So, we check whether we have enough uncolored (grey) coins as inputs
    -- to cover all new colored coins that are created.
    txColors = M.keys $ M.delete (getColor grey) (inputs <> outputs)
    extraGreyNeeded color =
        max 0 (colorAmount color outputs - colorAmount color inputs)
    greyNeededForPainting = sum (map extraGreyNeeded txColors)

-- | Removes from input/output lists elements whose coins are either
-- zero or negative, and if either (or both) the input/output lists
-- becomes empty, or was so from the start, the function returns
-- `Nothing`.

canonizeTx :: Transaction -> Maybe Transaction
canonizeTx tx@Transaction {..}
    | null txInputs || null txOutputs = Nothing
    | not (validateTxSize tx) = Nothing
    | otherwise = Just $ Transaction txInputsCanonized txOutputsCanonized
  where
    txInputsCanonized = filter ((> 0) . coinAmount . view _3) txInputs
    txOutputsCanonized = filter ((> 0) . coinAmount . snd) txOutputs

-- | Validates that signature is issued by public key associated with given
-- address for the transaction.
validateSignature :: Signature Transaction -> Address -> Transaction -> Bool
validateSignature signature (Address pk) = verify pk signature

-- | Given address and transaction returns total amount of money
-- transaction transfers to address.
getAmountByAddress :: Address -> Transaction -> M.IntMap Coin
getAmountByAddress addr Transaction{..} =
    let pair c = (getColor $ coinColor c, c) in
    M.fromListWith (+) $ map (pair . snd) $ filter ((==) addr . fst) txOutputs

-- | Given address a and transaction returns all addrids that have
-- address equal to a.
getAddrIdByAddress :: Address -> Transaction -> [AddrId]
getAddrIdByAddress addr transaction@Transaction{..} =
    let h = hash transaction in
    map (\(i,(_,c)) -> (h,i,c)) $
        filter ((==) addr . fst . snd) $ [(0 :: Int)..] `zip` txOutputs

-- | For each color, computes optimal usage of addrids to pay the given amount of
-- coins. Sum of coins of those addrids should be greater
-- or equal to given value, for each color. Here 'optimal' stands for 'trying to
-- include as many addrids as possible', so that means function takes
-- addrids with smaller amount of money first.
chooseAddresses :: [AddrId] -> M.IntMap Coin -> Maybe (M.IntMap ([AddrId], Coin))
chooseAddresses addrids valueMap =
    chooseOptimal addrids' (view _3) valueMap'
    where addrids' = filter ((/=0) . coinAmount . view _3) addrids
          valueMap' = M.filter ((/=0) . coinAmount) valueMap

chooseOptimal
    :: forall a.
       [a]                             -- ^ Elements we're choosing from
    -> (a -> Coin)                     -- ^ Getter of coins from the element
    -> M.IntMap Coin                -- ^ Map with amount of coins for each color
    -> Maybe (M.IntMap ([a], Coin)) -- ^ Map with chosen elements and change for each color
                                       -- If nothing, value can't be chosen (no money)
chooseOptimal addrids coinGetter valueMap =
    -- In case there are less colors in addrList than in valueList
    -- filler coins are added to short-circuit the comparison of lists.
    assert
        (map (sum . map coinGetter) addrList ++ repeat (Coin 0 0) >= M.elems valueMap) $
    M.fromList <$> mapM
        (\(color, value) ->
              (color,) <$> chooseHelper (M.findWithDefault [] color addrMap) value)
        (M.toList valueMap)
  where
    -- List of lists of addrids. Each sublist has the same color
    -- and the extern list is sorted by it. Inner list of the same
    -- color is sorted by coins amount.
    addrList :: [[a]]
    addrList =
        groupBy ((==) `on` (coinColor . coinGetter)) $
        sortBy (comparing (coinColor . coinGetter)) $
        sortBy (comparing (coinAmount . coinGetter)) addrids
    -- addrMap :: M.Map Color [a]
    -- Map from each color to addrids with a coin of that color
    addrMap = M.fromList $ map ((getColor . coinColor . coinGetter . head) &&& id) addrList
    -- chooseHelper :: [a] -> Coin -> ([a], Coin)
    -- This function goes through a list of addrids and calculates the optimal
    chooseHelper list value =
        -- choice of addrids and the coins that are left
        let foldFoo o@(_,_,Just _) _ = o
            foldFoo (accum,values,Nothing) e =
                let val = coinGetter e
                    newAccum = accum + val
                    newValues = e : values
                in ( newAccum
                   , newValues
                   , if newAccum >= value
                     then Just $ newAccum - value
                     else Nothing)
        in case foldl' foldFoo (Coin (coinColor value) 0, [], Nothing) list of
                    (_,chosenAIds,Just whatsLeft) -> Just (chosenAIds, whatsLeft)
                    (_,_,Nothing) -> Nothing

-- | This function creates for every address ∈ S_{out} a pair
-- (addr,addrid), where addrid is exactly a usage of this address in
-- this transasction
computeOutputAddrids :: Transaction -> [(AddrId, Address)]
computeOutputAddrids tx@Transaction{..} =
    let h = hash tx in
    map (\((addr, coin), i) -> ((h, i, coin), addr)) $ txOutputs `zip` [0..]
