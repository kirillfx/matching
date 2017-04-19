-- |

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TemplateHaskell   #-}

module Trading.Exchange.Order where

import           Control.Arrow          ((&&&))
import           Control.DeepSeq
import           Control.Lens
import           Data.Function          (on)
import           Data.Time
import           GHC.Generics
import           Trading.Exchange.Types


class OrderLike a where
  getOrderId :: a -> OrderId
  getOrderPrice :: a -> Price
  getOrderSize :: a -> Size
  getOrderCreationTime :: a -> UTCTime
  getOrderSide :: a -> Side
  modifySize :: a -> Size -> a


data Order (d :: Side) =
  Order { _orderId           :: !OrderId
        , _orderPrice        :: !Price
        , _orderSize         :: !Size
        , _orderCreationTime :: !UTCTime
        } deriving Show


makeLenses ''Order


instance Eq (Order d) where
  (==) = (==) `on` _orderId


instance Ord (Order d) where
  compare = compare `on` (_orderPrice &&& _orderId)


instance OrderLike (Order 'BUY) where
  getOrderId = _orderId
  getOrderPrice = _orderPrice
  getOrderSize = _orderSize
  getOrderCreationTime = _orderCreationTime
  getOrderSide = const BUY
  modifySize x v = set orderSize v x


instance OrderLike (Order 'SELL) where
  getOrderId = _orderId
  getOrderPrice = _orderPrice
  getOrderSize = _orderSize
  getOrderCreationTime = _orderCreationTime
  getOrderSide = const SELL
  modifySize x v = set orderSize v x


instance NFData (Order 'BUY) where
  rnf = rnf


instance NFData (Order 'SELL) where
  rnf = rnf
