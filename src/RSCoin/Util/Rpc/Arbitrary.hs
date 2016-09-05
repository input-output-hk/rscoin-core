-- | Arbitrary instances for WorkMode types.

module RSCoin.Util.Rpc.Arbitrary
       (
       ) where

import           Control.Monad.Random.Class (getRandomR)
import           Data.Time.Units            (fromMicroseconds)
import           System.Random              (StdGen, mkStdGen)
import           Test.QuickCheck            (Arbitrary (arbitrary))

import qualified RSCoin.Util.Rpc.PureRpc    as PureRpc

instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary

instance Arbitrary PureRpc.Delays where
    arbitrary =
        pure $
        PureRpc.Delays
            (\_ _ -> Just . fromMicroseconds <$> getRandomR (0, 1000))
