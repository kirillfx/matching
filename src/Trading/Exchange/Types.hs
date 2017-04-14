-- | Types and structures deifinitions.

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}

module Trading.Exchange.Types where

import           Data.Binary     (Binary (..))
import           Data.Function   (on)
import           Data.Scientific
import           Data.Time
import           GHC.Generics


type OrderId = Integer
type Price = Scientific
type Size = Scientific
type TradeID = Integer
type ProductID = Integer
type Fee = Scientific


-- | Environment type for storing all static information about product service.
-- Timestamp fixed here because of latency between FrontendService and
-- Product service.
data MatchingEnv =
  MatchingEnv { productId     :: !ProductID
              , productName   :: !String
              , executionTime :: !UTCTime -- ^ Fixed execution time.
              } deriving (Generic, Show)


instance Eq MatchingEnv where
  (==) = (==) `on` productId


-- | Direction of Order, Trade.
data Side = BUY | SELL deriving (Generic, Show, Eq)


-- | Intermidiate result of matching.
data MatchResult a b
  = AgressorLeft !a
  | CoAgressorLeft !b
  | BothMatched
  | NotMatched !a !b
  deriving (Generic, Eq, Show)


-- | Result of mathcing of 2 Orders.
data Trade =
  Trade { tradeId        :: !TradeID
        , tradeProductId :: !ProductID
        , tradePriec     :: !Price
        , tradeSize      :: !Size
        , tradeFee       :: !Fee
        , tradeTime      :: !UTCTime
        , tradeDirection :: !Side
        } deriving (Generic, Show)


instance Eq Trade where
  (==) = (==) `on` tradeId


-- | Log of all activities.
data OrderlogRecord
  = New
  | Canceled
  | Change
  deriving (Generic, Show)
