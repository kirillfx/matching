-- | Types and structures deifinitions.

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}

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
-- Timestamp fixed here because of latency between FrontendService and
-- Product service.
data MatchingEnv =
  MatchingEnv { productId     :: ProductID
              , productName   :: String
              , executionTime :: UTCTime -- ^ Fixed execution time.
              } deriving (Generic, Show)


instance Eq MatchingEnv where
  (==) = (==) `on` productId


type OrderId = Integer
type Price = Scientific
type Size = Scientific
type TradeID = Integer
type ProductID = Integer
type Fee = Scientific


-- | Direction of Order, Trade.
data Side = BUY | SELL deriving (Generic, Show, Eq)


data Order2 (d :: Side) =
  Order { _orderId           :: OrderId
        , _orderPrice        :: Price
        , _orderSize         :: Size
        , _orderCreationTime :: UTCTime
        } deriving (Generic, Show)



-- | Accessor methods.
class Order a where
  orderID :: a -> OrderId
  orderPrice :: a -> Price
  orderSize :: a -> Size
  orderCreationTime :: a -> UTCTime


data Buy = Buy OrderId Price Size UTCTime deriving (Generic, Show)


data Sell = Sell OrderId Price Size UTCTime deriving (Generic, Show)


instance Order Buy where
  orderID (Buy oid p s t) = oid
  orderPrice (Buy oid p s t) = p
  orderSize (Buy oid p s t) = s
  orderCreationTime (Buy oid p s t) = t


instance Order Sell where
  orderID (Sell oid p s t) = oid
  orderPrice (Sell oid p s t) = p
  orderSize (Sell oid p s t) = s
  orderCreationTime (Sell oid p s t) = t


-- | Data structure for storing both directions
data OrderBook =
  OrderBook { asks :: Map Scientific (Seq Sell)
            , bids :: Map Scientific (Seq Buy)
            } deriving (Generic, Show)


-- | Intermidiate result of matching.
data MatchResult a b
  = AgressorLeft a
  | CoAgressorLeft b
  | BothMatched
  | NotMatched a b
  deriving (Generic, Eq, Show)


-- | Result of mathcing of 2 Orders.
data Trade =
  Trade { tradeId        :: TradeID
        , tradeProductId :: ProductID
        , tradePriec     :: Price
        , tradeSize      :: Size
        , tradeFee       :: Fee
        , tradeTime      :: UTCTime
        , tradeDirection :: Side
        } deriving (Generic, Show)


instance Eq Trade where
  (==) = (==) `on` tradeId


-- | Log of all activities.
data OrderlogRecord
  = New
  | Canceled
  | Change
  deriving (Generic, Show)
