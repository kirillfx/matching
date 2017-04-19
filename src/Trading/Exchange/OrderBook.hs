-- | OrderBook data structure definition.

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Trading.Exchange.OrderBook where

import           Control.DeepSeq
import           Control.Lens           hiding ((<|), (|>))
import           Data.HashMap.Strict    (HashMap)
import           Data.Monoid            ((<>))
import           Data.Set               (Set)
import qualified Data.Set               as S
import           GHC.Generics
import           Trading.Exchange.Order
import           Trading.Exchange.Types


-- | Data structure for storing both directions
data OrderBook =
  OrderBook { _orderBookAsks     :: Set (Order 'SELL)
            , _orderBookBids     :: Set (Order 'BUY)
            , _registry          :: HashMap OrderId (Side,Price)
            , _nextOrderlogIndex :: Integer
            , _nextTradeId       :: TradeId
            } deriving (Generic, Show)


instance NFData OrderBook


emptyOrderBook :: OrderBook
emptyOrderBook = OrderBook mempty mempty mempty 0 0


makeLenses ''OrderBook


insertBuy :: Order 'BUY -> OrderBook -> OrderBook
insertBuy x book = book & orderBookBids %~ (S.insert x)
                        & registry.at (x^.orderId) ?~ (BUY, x^.orderPrice)


insertSell :: Order 'SELL -> OrderBook -> OrderBook
insertSell x book = book & orderBookAsks %~ (S.insert x)
                         & registry.at (x^.orderId) ?~ (SELL, x^.orderPrice)

insertOrder :: Either (Order 'SELL) (Order 'BUY)
            -> OrderBook
            -> OrderBook
insertOrder (Left x) book =
  book & orderBookAsks %~ (S.insert x)
       & registry.at (x^.orderId) ?~ (SELL, x^.orderPrice)
insertOrder (Right x) book =
  book & orderBookBids %~ (S.insert x)
       & registry.at (x^.orderId) ?~ (BUY, x^.orderPrice)


cancelOrder :: OrderId -> OrderBook -> (OrderBook, Either String (OrderId,Side,Price))
cancelOrder oid book = case book^.registry.at oid of

  Nothing -> (book, Left $ show oid <> "Not presend")

  Just (side,price) -> case side of

    BUY -> (book & orderBookBids  %~ (S.filter (not . (== oid) . _orderId))
                 & registry.at oid .~ Nothing, Right (oid, side, price))

    SELL -> (book & orderBookAsks %~ (S.filter (not . (== oid) . _orderId))
                  & registry.at oid .~ Nothing, Right (oid, side, price))
