-- | Matching algorithm.

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Trading.Exchange.Matching where

import           Control.Lens
import           Control.Monad.RWS.Strict
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Trading.Exchange.Order
import           Trading.Exchange.OrderBook
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
  -- TODO: Orderlog
  return e


class Matching a b | a -> b, b -> a where

  drawBestCoOrder :: Base (Maybe b)

  match :: a -> b -> Base (MatchResult a b, Trade)

  insertM :: a -> Base ()

  insertM' :: b -> Base ()

  processMatchResult :: MatchResult a b -> Base [Trade]
  processMatchResult (AgressorLeft x)   = matchRecursively x
  processMatchResult (CoAgressorLeft x) = insertM' x >> return []
  processMatchResult BothMatched        = return []
  processMatchResult (NotMatched a b)   = insertM a >> insertM' b >> return []


  matchRecursively :: a -> Base [Trade]
  matchRecursively order = do
    mbCoOrder <- drawBestCoOrder
    case mbCoOrder of
      Nothing -> insertM order >> return []
      Just coOrder -> do
        (r,t) <- match order coOrder
        rs <- processMatchResult r
        return (t:rs)


instance Matching (Order 'BUY) (Order 'SELL) where

  drawBestCoOrder = do
    book <- get
    let mb  = S.minView (book^.orderBookAsks)
    case mb of
      Nothing -> return Nothing
      Just (x, xs) -> do
        modify (set orderBookAsks xs)
        return $ Just x

  match (Order oid1 p1 v1 t1) (Order oid2 p2 v2 t2) =
    case compare v1 v2 of

    LT -> do
      let v' = v2 - v1

      -- Agressor's Orderlog
      oid <- getNextOrderlogIndex
      --tell [Execution oid t1 s1 i1 BUY p1 (Volume 0) tid p2]

      -- CoAgressor's Orderlog
      oid' <- getNextOrderlogIndex
      --tell [Execution oid' t1 s2 i2 SELL p2 v' tid p2]

      tid <- getNextTradeId
      pid <- asks productId
      f <- asks fee

      return ( CoAgressorLeft $ Order oid2 p2 v' t2
             , Trade tid pid p2 v1 f t1 BUY)

    EQ -> do
      -- Agressor's Orderlog
      oid <- getNextOrderlogIndex
      -- tell [Execution oid t1 s1 i1 BUY p1 (Volume 0) tid p2]

      -- CoAgressor's Orderlog
      oid' <- getNextOrderlogIndex
      -- tell [Execution oid' t1 s2 i2 SELL p2 (Volume 0) tid p2]

      tid <- getNextTradeId
      pid <- asks productId
      f <- asks fee

      return ( BothMatched, Trade tid pid p2 v1 f t1 BUY)

    GT -> do
      let v' = v1 - v2

      -- Agressor's Orderlog
      oid <- getNextOrderlogIndex
      -- tell [Execution oid t1 s1 i1 BUY p1 v' tid p2]

      -- CoAgressor's Orderlog
      oid' <- getNextOrderlogIndex
      -- tell [Execution oid' t1 s2 i2 SELL p2 (Volume 0) tid p2]

      tid <- getNextTradeId
      pid <- asks productId
      f <- asks fee

      return ( AgressorLeft $ Order oid1 p1 v' t1
             , Trade tid pid p2 v2 f t1 BUY)

  insertM buy = modify (insertBuy buy)

  insertM' sell = modify (insertSell sell)


instance Matching (Order 'SELL) (Order 'BUY) where

  drawBestCoOrder = do
    book <- get
    let mb  = S.maxView (book^.orderBookBids)
    case mb of
      Nothing -> return Nothing
      Just (x, xs) -> do
        modify (set orderBookBids xs)
        return $ Just x

  match (Order oid1 p1 v1 t1) (Order oid2 p2 v2 t2) =
    case compare v1 v2 of

    LT -> do
      let v' = v2 - v1

      -- Agressor's Orderlog
      oid <- getNextOrderlogIndex
      --tell [Execution oid t1 s1 i1 BUY p1 (Volume 0) tid p2]

      -- CoAgressor's Orderlog
      oid' <- getNextOrderlogIndex
      --tell [Execution oid' t1 s2 i2 SELL p2 v' tid p2]

      tid <- getNextTradeId
      pid <- asks productId
      f <- asks fee

      return ( CoAgressorLeft $ Order oid2 p2 v' t2
             , Trade tid pid p2 v1 f t1 SELL)

    EQ -> do
      -- Agressor's Orderlog
      oid <- getNextOrderlogIndex
      -- tell [Execution oid t1 s1 i1 BUY p1 (Volume 0) tid p2]

      -- CoAgressor's Orderlog
      oid' <- getNextOrderlogIndex
      -- tell [Execution oid' t1 s2 i2 SELL p2 (Volume 0) tid p2]

      tid <- getNextTradeId
      pid <- asks productId
      f <- asks fee

      return ( BothMatched, Trade tid pid p2 v1 f t1 SELL)

    GT -> do
      let v' = v1 - v2

      -- Agressor's Orderlog
      oid <- getNextOrderlogIndex
      -- tell [Execution oid t1 s1 i1 BUY p1 v' tid p2]

      -- CoAgressor's Orderlog
      oid' <- getNextOrderlogIndex
      -- tell [Execution oid' t1 s2 i2 SELL p2 (Volume 0) tid p2]

      tid <- getNextTradeId
      pid <- asks productId
      f <- asks fee

      return ( AgressorLeft $ Order oid1 p1 v' t1
             , Trade tid pid p2 v2 f t1 SELL)

  insertM sell = modify (insertSell sell)

  insertM' buy  = modify (insertBuy buy)


-- | Top level function
matchOrder :: (Matching a b, Matching b a) => Either a b -> Base [Trade]
matchOrder e = case e of
  Left sell -> matchRecursively sell
  Right buy -> matchRecursively buy
