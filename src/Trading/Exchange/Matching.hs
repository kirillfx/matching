-- | Matching algorithm.

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Trading.Exchange.Matching where

import           Control.Lens
import           Control.Monad.RWS.Strict
import qualified Data.Set                        as S
import           Trading.Exchange.MatchResult
import           Trading.Exchange.Order
import           Trading.Exchange.OrderBook
import           Trading.Exchange.OrderlogRecord
import           Trading.Exchange.Trade
import           Trading.Exchange.Types

type Base = RWS MatchingEnv [OrderlogRecord] OrderBook


getNextOrderlogIndex :: Base Integer
getNextOrderlogIndex = do
  i <- gets _nextOrderlogIndex
  nextOrderlogIndex %= succ
  return i

getNextTradeId :: Base Integer
getNextTradeId = do
  i <- gets _nextTradeId
  nextTradeId %= succ
  return i


-- | Delete Order by OrderId
cancelOrderM :: OrderId -> Base (Either String OrderId)
cancelOrderM oid = do
  book <- get
  let (book', e) = cancelOrder oid book
  put book'
  case e of
    Left x -> return $ Left x
    Right (_,s,p) -> do
      logId <- getNextOrderlogIndex
      t <- asks executionTime
      pid <- asks productId
      tell [Deletion logId t pid s oid p 0]
      return $ Right oid


class (OrderLike a, OrderLike b) => Matching a b | a -> b, b -> a where

  drawBestCoOrder :: Base (Maybe b)

  isFillableBy :: a -> b -> Bool

  match :: a -> b -> Base (MatchResult a b)
  match a b =
    if not $ isFillableBy a b
    then return (NotMatched a b)
    else do
      tid <- getNextTradeId
      pid <- asks productId
      f <- asks fee

      case compare v1 v2 of

        LT -> do
          let v' = v2 - v1
          -- Agressor's Orderlog
          oid <- getNextOrderlogIndex
          tell [Execution oid t1 pid i1 (getOrderSide a) p1 0 tid p2]
          -- CoAgressor's Orderlog
          oid' <- getNextOrderlogIndex
          tell [Execution oid' t1 pid i2 (getOrderSide b) p2 v' tid p2]

          return ( CoAgressorLeft (modifySize b v') (Trade tid pid p2 v1 f t1 side))

        EQ -> do
          -- Agressor's Orderlog
          oid <- getNextOrderlogIndex
          tell [Execution oid t1 pid i1 (getOrderSide a) p1 0 tid p2]
          -- CoAgressor's Orderlog
          oid' <- getNextOrderlogIndex
          tell [Execution oid' t1 pid i2 (getOrderSide b) p2 0 tid p2]

          modify (registry.at i2 .~ Nothing)

          return ( BothMatched (Trade tid pid p2 v1 f t1 side))

        GT -> do
          let v' = v1 - v2
          -- Agressor's Orderlog
          oid <- getNextOrderlogIndex
          tell [Execution oid t1 pid i1 BUY p1 v' tid p2]
          -- CoAgressor's Orderlog
          oid' <- getNextOrderlogIndex
          tell [Execution oid' t1 pid i2 SELL p2 0 tid p2]

          modify (registry.at i2 .~ Nothing)

          return ( AgressorLeft (modifySize a v') (Trade tid pid p2 v2 f t1 side))
      where
        side = getOrderSide a
        i1 = getOrderId a
        i2 = getOrderId b
        p1 = getOrderPrice a
        p2 = getOrderPrice b
        v1 = getOrderSize a
        v2 = getOrderSize b
        t1 = getOrderCreationTime a

  insertM :: a -> Base ()

  insertM' :: b -> Base ()

  processMatchResult :: MatchResult a b -> Base [Trade]
  processMatchResult (AgressorLeft x t)   = (:) <$> pure t <*> (matchRecursively x)
  processMatchResult (CoAgressorLeft x t) = insertM' x >> return [t]
  processMatchResult (BothMatched t)      = return [t]
  processMatchResult (NotMatched a b)   = insertM a >> insertM' b >> return []


  matchRecursively :: a -> Base [Trade]
  matchRecursively order = do
    mbCoOrder <- drawBestCoOrder
    oid <- getNextOrderlogIndex
    pid <- asks productId
    f <- asks fee

    tell [Insertion oid t pid side i1 p1 v1]
    case mbCoOrder of
      Nothing -> insertM order >> return []
      Just coOrder -> do
        match order coOrder >>= processMatchResult
    where
      side = getOrderSide order
      t = getOrderCreationTime order
      i1 = getOrderId order
      p1 = getOrderPrice order
      v1 = getOrderSize order



instance Matching (Order 'BUY) (Order 'SELL) where

  isFillableBy buy sell = buy^.orderPrice >= sell^.orderPrice

  drawBestCoOrder = do
    book <- get
    let mb  = S.minView (book^.orderBookAsks)
    case mb of
      Nothing -> return Nothing
      Just (x, xs) -> do
        orderBookAsks .= xs
        return $ Just x

  insertM buy = modify (insertBuy buy)

  insertM' sell = modify (insertSell sell)


instance Matching (Order 'SELL) (Order 'BUY) where

  isFillableBy sell buy = sell^.orderPrice <= buy^.orderPrice

  drawBestCoOrder = do
    book <- get
    let mb  = S.maxView (book^.orderBookBids)
    case mb of
      Nothing -> return Nothing
      Just (x, xs) -> do
        orderBookBids .= xs
        return $ Just x

  insertM sell = modify (insertSell sell)

  insertM' buy  = modify (insertBuy buy)


-- | Top level function
matchOrder :: (Matching a b, Matching b a) => Either a b -> Base [Trade]
matchOrder e = case e of
  Left sell -> matchRecursively sell
  Right buy -> matchRecursively buy
