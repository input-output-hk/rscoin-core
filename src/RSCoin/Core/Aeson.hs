{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Default aeson instances.

module RSCoin.Core.Aeson
       ( UtxoBalances (..)
       , utxoBalances
       ) where

import           Data.Aeson             (FromJSON (..), ToJSON, object, toJSON, (.:),
                                         (.=))
import           Data.Aeson.TH          (defaultOptions, deriveJSON)
import           Data.Aeson.Types       (Value (Object), typeMismatch)
import qualified Data.HashMap.Strict    as HM

import           RSCoin.Core.Arbitrary  (SmallAllocationStrategy, SmallTransaction,
                                         SmallTxStrategy)
import           RSCoin.Core.Primitives (Address (..), Coin (..), CoinAmount (..), Color,
                                         Transaction)
import           RSCoin.Core.Strategy   (AllocationAddress, AllocationStrategy,
                                         PartyAddress, TxStrategy)
import           RSCoin.Core.Types      (Utxo)

$(deriveJSON defaultOptions ''Address)
$(deriveJSON defaultOptions ''AllocationAddress)
$(deriveJSON defaultOptions ''AllocationStrategy)
$(deriveJSON defaultOptions ''Coin)
$(deriveJSON defaultOptions ''CoinAmount)
$(deriveJSON defaultOptions ''Color)
$(deriveJSON defaultOptions ''PartyAddress)
$(deriveJSON defaultOptions ''Transaction)
$(deriveJSON defaultOptions ''TxStrategy)

$(deriveJSON defaultOptions ''SmallAllocationStrategy)
$(deriveJSON defaultOptions ''SmallTransaction)
$(deriveJSON defaultOptions ''SmallTxStrategy)

-- | NEEDED ONLY FOR CSL AND POSTVEND APPLICATIOИ!!1!
newtype UtxoBalances = UtxoBalances
    { getUtxoBalances :: [(Address, CoinAmount)]
    }

utxoBalances :: Utxo -> UtxoBalances
utxoBalances = UtxoBalances . map toAddrCoin . HM.toList
  where
    toAddrCoin ((_, _, coin), addr) = (addr, coinAmount coin)

instance ToJSON UtxoBalances where
    toJSON = toJSON . map convert . getUtxoBalances
      where
        convert (addr, coinAmount) =
            object
                [ "address" .= getAddress addr
                , "coin" .= (realToFrac :: CoinAmount -> Double) coinAmount
                ]

data UtxoParsingHelper = UtxoParsingHelper
    { uphAddress :: !Address
    , uphCoin    :: !CoinAmount
    }

instance FromJSON UtxoParsingHelper where
    parseJSON (Object obj) =
        UtxoParsingHelper <$> (Address <$> obj .: "address") <*>
        ((realToFrac :: Double -> CoinAmount) <$> obj .: "coin")
    parseJSON v = typeMismatch "UtxoParsingHelper" v

instance FromJSON UtxoBalances where
    parseJSON v = UtxoBalances . map convert <$> parseJSON v
      where
        convert UtxoParsingHelper {..} = (uphAddress, uphCoin)
