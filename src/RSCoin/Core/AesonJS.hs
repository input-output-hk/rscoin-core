{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Aeson instances.
-- Instances that communicate with Javascript. They mostly have numbers converted to string

module RSCoin.Core.AesonJS
       ( UtxoAsBalances (..)
       ) where

import           Data.Aeson             (FromJSON (..), ToJSON, object, toJSON, (.=))
import           Data.Aeson.TH          (deriveJSON)
import           Data.Aeson.Types       (Value (Number), typeMismatch)
import qualified Data.HashMap.Strict    as HM
import           Data.Scientific        (toRealFloat)

import           Serokell.Aeson.Options (defaultOptionsPS)

import           RSCoin.Core.Primitives (Address (..), Coin, CoinAmount (..), Color,
                                         Transaction)
import           RSCoin.Core.Strategy   (AllocationAddress, AllocationStrategy,
                                         PartyAddress, TxStrategy)
import           RSCoin.Core.Types      (HBlockMetadata, Utxo, WithMetadata)

instance ToJSON CoinAmount where
    toJSON = toJSON . (realToFrac :: CoinAmount -> Double)

instance FromJSON CoinAmount where
    parseJSON (Number n) =
        pure . (realToFrac :: Double -> CoinAmount) . toRealFloat $ n
    parseJSON val = typeMismatch "CoinAmount" val

$(deriveJSON defaultOptionsPS ''Address)
$(deriveJSON defaultOptionsPS ''AllocationAddress)
$(deriveJSON defaultOptionsPS ''AllocationStrategy)
$(deriveJSON defaultOptionsPS ''Coin)
$(deriveJSON defaultOptionsPS ''Color)
$(deriveJSON defaultOptionsPS ''HBlockMetadata)
$(deriveJSON defaultOptionsPS ''PartyAddress)
$(deriveJSON defaultOptionsPS ''Transaction)
$(deriveJSON defaultOptionsPS ''TxStrategy)
$(deriveJSON defaultOptionsPS ''WithMetadata)

newtype UtxoAsBalances = UtxoAsBalances
    { getUtxoAsBalances :: Utxo
    }

instance ToJSON UtxoAsBalances where
    toJSON = toJSON . map toAddrCoin . HM.toList . getUtxoAsBalances
      where
        toAddrCoin ((_, _, coin), addr) =
            object ["address" .= getAddress addr, "coin" .= coin]
