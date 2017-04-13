-- | Types and structures deifinitions.

{-# LANGUAGE DeriveGeneric #-}

module Trading.Exchange.Types where

import           Data.Binary     (Binary (..))
import           Data.Function   (on)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Scientific
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as S
import           Data.Time
import           GHC.Generics


-- | Environment type for storing all static information about product service.
data MatchingEnv = MatchingEnv { productId   :: ProductID
                               , productName :: String
                               } deriving Show


instance Eq MatchingEnv where
  (==) = (==) `on` productId


type OrderID = Integer
type Price = Scientific
type Volume = Scientific
type TradeID = Integer
type ProductID = Integer
type Fee = Scientific


-- | Direction of Order, Trade.
data Side = BUY | SELL deriving (Show, Eq)


-- | Initial and simplies approach to Order type
data Order = Order OrderID Price Volume Side deriving Show


-- | Data structure for storing both directions
data OrderBook =
  OrderBook { asks :: Map Scientific (Seq Order)
            , bids :: Map Scientific (Seq Order)
            } deriving Show


drawBestAsk :: OrderBook -> (Order, OrderBook)
drawBestAsk = undefined


drawBestBid :: OrderBook -> (Order, OrderBook)
drawBestBid = undefined


-- | Intermidiate result of matching.
data MatchResult
  = AgressorLeft Order
  | CoAgressorLeft Order
  | BothMatched
  deriving Show


-- | Result of mathcing of 2 Orders.
data Trade = Trade TradeID ProductID Price Volume Fee UTCTime Side


-- | Log of all activities.
data OrderlogRecord
  = New
  | Canceled
  | Change
  deriving (Show)
