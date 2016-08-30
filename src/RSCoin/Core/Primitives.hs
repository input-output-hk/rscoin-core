{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The most basic primitives from the paper.

module RSCoin.Core.Primitives
       ( Color (..)
       , CoinAmount (..)
       , Coin (..)
       , Address (..)
       , AddrId
       , Transaction (..)
       , TransactionId
       , grey
       ) where

import           Data.Binary         (Binary)
import           Data.Data           (Data)
import           Data.Hashable       (Hashable)
import           Data.Ord            (comparing)
import           Data.SafeCopy       (base, deriveSafeCopy)
import qualified Data.Text.Buildable as B (Buildable (build))
import           Formatting          (bprint, build, float, int, (%))
import           GHC.Generics        (Generic)

import           Serokell.Util.Text  (listBuilderJSON, pairBuilder)

import           RSCoin.Core.Crypto  (Hash, PublicKey, hash)

newtype Color = Color
    { getC :: Int
    } deriving (Show,Eq,Enum,Real,Ord,Num,Integral,Hashable,Binary,B.Buildable,Generic,Data)

-- | Predefined color. Grey is for uncolored coins,
-- Purple is for multisignature address allocation.
grey :: Color
grey = Color 0

newtype CoinAmount = CoinAmount
    { getAmount :: Rational
    } deriving (Eq,Show,Ord,Num,Fractional,RealFrac,Real,Data,B.Buildable,Hashable,Binary)

-- | Coin is the least possible unit of currency.
-- We use very simple model at this point.
data Coin = Coin
    { getColor :: Color
    , getCoin  :: CoinAmount
    } deriving (Show,Eq,Ord,Generic,Data)

reportError :: String -> Coin -> Coin -> a
reportError s c1 c2 =
        error $ "Error: " ++ s ++
                " of coins with different colors: " ++
                show c1 ++ " " ++ show c2

instance Binary Coin

instance Hashable Coin

instance B.Buildable Coin where
    build (Coin col c) =
        bprint (float % " coin(s) of color " % int) valueDouble col
      where
        valueDouble :: Double
        valueDouble = fromRational $ getAmount c

instance Num Coin where
    (+) c1@(Coin col c) c2@(Coin col' c')
      | col == col' = Coin col (c + c')
      | otherwise = reportError "sum" c1 c2
    (*) c1@(Coin col c) c2@(Coin col' c')
      | col == col' = Coin col (c * c')
      | otherwise = reportError "product" c1 c2
    (-) c1@(Coin col c) c2@(Coin col' c')
      | col == col' = Coin col (c - c')
      | otherwise = reportError "subtraction" c1 c2
    abs (Coin a b) = Coin a (abs b)
    signum (Coin col c) = Coin col (signum c)
    fromInteger c = Coin grey (fromInteger c)

-- | Address can serve as input or output to transactions.
-- It is simply a public key.
newtype Address = Address
    { getAddress :: PublicKey
    } deriving (Show,Ord,B.Buildable,Eq,Hashable,Generic,Binary)

-- | AddrId identifies usage of address as output of transaction.
-- Basically, it is tuple of transaction identifier, index in list of outputs
-- and associated value.
type AddrId = (TransactionId, Int, Coin)

instance B.Buildable (TransactionId, Int, Coin) where
    build (t,i,c) = bprint template t i c
      where template = "Addrid { transactionId = " % build %
                       ", index = " % build % ", coins = " % build % " }"

-- | Transaction represents act of transfering units of currency from
-- set of inputs to set of outputs.
data Transaction = Transaction
    { txInputs  :: ![AddrId]
    , txOutputs :: ![(Address, Coin)]
    } deriving (Show, Eq, Generic)

instance Ord Transaction where
    compare = comparing hash

instance Binary Transaction

instance Hashable Transaction

instance B.Buildable Transaction where
    build Transaction{..} =
        bprint
            template
            (listBuilderJSON $ map B.build txInputs)
            (listBuilderJSON $ map pairBuilder txOutputs)
      where
        template = "Transaction { inputs = " % build % ", outputs = " % build % " }"

-- | Transaction is identified by its hash
type TransactionId = Hash Transaction

$(deriveSafeCopy 0 'base ''Address)
$(deriveSafeCopy 0 'base ''Color)
$(deriveSafeCopy 0 'base ''CoinAmount)
$(deriveSafeCopy 0 'base ''Coin)
$(deriveSafeCopy 0 'base ''Transaction)
