{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Aeson instances.
-- Instances that communicate with Javascript. They mostly have numbers converted to string

module RSCoin.Core.AesonJS
       ( JS (..)
       ) where

import           Data.Aeson             (FromJSON (..), ToJSON, toJSON)
import           Data.Aeson.TH          (deriveJSON)
import           Data.Aeson.Types       (Value (..))
import           Data.Monoid            ((<>))

import           Serokell.Aeson.Options (defaultOptionsPS)
import           Serokell.Util.Text     (readFractional, showFixedPretty')

import           RSCoin.Core.Primitives (Address, Coin, CoinAmount (..), Color,
                                         Transaction)
import           RSCoin.Core.Strategy   (AllocationAddress, AllocationStrategy,
                                         PartyAddress, TxStrategy)

newtype JS a = JS
    { getJS :: a
    } deriving (Show)

instance ToJSON (JS CoinAmount) where
    toJSON = String . showFixedPretty' 5 . getAmount . getJS

instance FromJSON (JS CoinAmount) where
    -- TODO: use Parser from Aeson to report error
    parseJSON (String v) =
        either
            (error . ("Can't parse `JS CoinAmount`: " <>))
            (pure . JS . CoinAmount) $
        readFractional v
    parseJSON _ = error "Expected `JS CoinAmount` to be represented as String"

$(deriveJSON defaultOptionsPS ''Address)
$(deriveJSON defaultOptionsPS ''AllocationAddress)
$(deriveJSON defaultOptionsPS ''AllocationStrategy)
$(deriveJSON defaultOptionsPS ''Coin)
$(deriveJSON defaultOptionsPS ''CoinAmount)
$(deriveJSON defaultOptionsPS ''Color)
$(deriveJSON defaultOptionsPS ''PartyAddress)
$(deriveJSON defaultOptionsPS ''Transaction)
$(deriveJSON defaultOptionsPS ''TxStrategy)
