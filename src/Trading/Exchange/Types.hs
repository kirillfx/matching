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
type TradeId = Integer
type ProductId = Integer
type Fee = Scientific


-- | Environment type for storing all static information about product service.
-- Timestamp fixed here because of latency between FrontendService and
-- Product service.
data MatchingEnv =
  MatchingEnv { productId     :: !ProductId
              , productName   :: !String
              , executionTime :: !UTCTime -- ^ Fixed execution time.
              , fee           :: Fee
              } deriving (Generic, Show)


instance Eq MatchingEnv where
  (==) = (==) `on` productId


-- | Direction of Order, Trade.
data Side = BUY | SELL deriving (Generic, Show, Eq)


-- | Intermidiate result of matching.
data MatchResult a b
  = AgressorLeft !a Trade
  | CoAgressorLeft !b Trade
  | BothMatched Trade
  | NotMatched !a !b
  deriving (Generic, Eq, Show)


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


-- | Log of all activities.
data OrderlogRecord
  = New
  | Canceled
  | Change
  deriving (Generic, Show)
