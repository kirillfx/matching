-- | OrderBook data structure definition.

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Trading.Exchange.OrderBook where

import           Control.Lens           hiding ((<|), (|>))
import           Data.HashMap.Strict    (HashMap)
import           Data.Monoid            ((<>))
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Trading.Exchange.Order
import           Trading.Exchange.Types


-- | Data structure for storing both directions
data OrderBook =
  OrderBook { _asks     :: Set (Order 'SELL)
            , _bids     :: Set (Order 'BUY)
            , _registry :: HashMap OrderId (Side,Price)
            } deriving (Show)


makeLenses ''OrderBook


insertOrder :: Either (Order 'SELL) (Order 'BUY)
            -> OrderBook
            -> OrderBook
insertOrder (Left x) book =
  book & asks %~ (S.insert x)
       & registry.at (x^.orderId) ?~ (SELL, x^.orderPrice)
insertOrder (Right x) book =
  book & bids %~ (S.insert x)
       & registry.at (x^.orderId) ?~ (BUY, x^.orderPrice)


cancelOrder :: OrderId -> OrderBook -> (OrderBook, Either String OrderId)
cancelOrder oid book = case book^.registry.at oid of

  Nothing -> (book, Left $ show oid <> "Not presend")

  Just (side,price) -> case side of

    BUY -> (book & bids  %~ (S.filter (not . (== oid) . _orderId))
                 & registry.at oid .~ Nothing, Right oid)

    SELL -> (book & asks %~ (S.filter (not . (== oid) . _orderId))
                  & registry.at oid .~ Nothing, Right oid)
