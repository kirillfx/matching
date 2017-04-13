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


type OrderId = Integer
type Price = Scientific
type Size = Scientific
type TradeID = Integer
type ProductID = Integer
type Fee = Scientific


-- | Direction of Order, Trade.
data Side = BUY | SELL deriving (Show, Eq)


class Order a where
  orderID :: a -> OrderId
  orderPrice :: a -> Price
  orderSize :: a -> Size


data Buy = Buy OrderId Price Size deriving Show

data Sell = Sell OrderId Price Size deriving Show


instance Order Buy where
  orderID (Buy oid p s) = oid
  orderPrice (Buy oid p s) = p
  orderSize (Buy oid p s) = s


instance Order Sell where
  orderID (Sell oid p s) = oid
  orderPrice (Sell oid p s) = p
  orderSize (Sell oid p s) = s


-- | Data structure for storing both directions
data OrderBook =
  OrderBook { asks :: Map Scientific (Seq Sell)
            , bids :: Map Scientific (Seq Buy)
            } deriving Show


-- | Intermidiate result of matching.
data MatchResult a b
  = AgressorLeft a
  | CoAgressorLeft b
  | BothMatched
  | NotMatched a b
  deriving (Eq, Show)


-- | Result of mathcing of 2 Orders.
data Trade = Trade TradeID ProductID Price Size Fee UTCTime Side


-- | Log of all activities.
data OrderlogRecord
  = New
  | Canceled
  | Change
  deriving (Show)
