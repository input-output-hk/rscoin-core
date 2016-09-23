{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Default aeson instances.

module RSCoin.Core.Aeson
       (
       ) where

import           Data.Aeson.TH          (defaultOptions, deriveJSON)

import           RSCoin.Core.Primitives (Address, Coin, CoinAmount (..), Color,
                                         Transaction)
import           RSCoin.Core.Strategy   (AllocationAddress, AllocationStrategy,
                                         PartyAddress, TxStrategy)

import           RSCoin.Core.Arbitrary  (SmallAllocationStrategy,
                                         SmallTransaction,
                                         SmallTxStrategy)

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
