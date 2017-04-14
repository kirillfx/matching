-- |

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Trading.Exchange.OrderBook where

import           Control.Lens           hiding ((<|), (|>))
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (maybe)
import           Data.Monoid            ((<>))
import           Data.Scientific
import           Data.Sequence          (Seq, (<|), (|>))
import qualified Data.Sequence          as S
import           Trading.Exchange.Order
import           Trading.Exchange.Types


-- | Data structure for storing both directions
data OrderBook =
  OrderBook { _asks     :: Map Scientific (Seq (Order 'SELL))
            , _bids     :: Map Scientific (Seq (Order 'BUY))
            , _registry :: HashMap OrderId (Side,Price)
            } deriving (Show)


makeLenses ''OrderBook


insertOrder :: Either (Order 'SELL) (Order 'BUY)
            -> OrderBook
            -> OrderBook
insertOrder (Left x) book =
  let price = view orderPrice x
  in case (book ^? asks.at price) of
    Nothing    -> book & asks.at price ?~ (S.singleton x)
                       & registry.at (x^.orderId) ?~ (SELL, x^.orderPrice)
    Just level -> book & asks.ix price %~ (|> x)
                       & registry.at (x^.orderId) ?~ (SELL, x^.orderPrice)
insertOrder (Right x) book =
  let price = view orderPrice x
  in case (book ^? bids.at price) of
    Nothing    -> book & bids.at price ?~ (S.singleton x)
                       & registry.at (x^.orderId) ?~ (BUY, x^.orderPrice)
    Just level -> book & bids.ix price %~ (|> x)
                       & registry.at (x^.orderId) ?~ (BUY, x^.orderPrice)



cancelOrder :: OrderId -> OrderBook -> (OrderBook, Either String OrderId)
cancelOrder oid book = case book^.registry.at oid of

  Nothing -> (book, Left $ show oid <> "Not presend")

  Just (side,price) -> case side of

    BUY -> (book & bids.ix price %~ (S.filter (not . (== oid) . _orderId))
                 & registry.at oid .~ Nothing, Right oid)

    SELL -> (book & asks.ix price %~ (S.filter (not . (== oid) . _orderId))
                  & registry.at oid .~ Nothing, Right oid)
