-- |

{-# LANGUAGE DeriveGeneric #-}

module Trading.Exchange.Trade where

import           Control.DeepSeq
import           Data.Function          (on)
import           Data.Time              (UTCTime)
import           GHC.Generics
import           Trading.Exchange.Types


-- | Result of mathcing of 2 Orders.
data Trade =
  Trade { tradeId        :: !TradeId
        , tradeProductId :: !ProductId
        , tradePrice     :: !Price
        , tradeSize      :: !Size
        , tradeFee       :: !Fee
        , tradeTime      :: !UTCTime
        , tradeDirection :: !Side
        } deriving (Generic, Show)


instance Eq Trade where
  (==) = (==) `on` tradeId


instance NFData Trade
