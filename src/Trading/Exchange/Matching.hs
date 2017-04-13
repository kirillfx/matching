-- | Matching algorithm.
-- Generalized:
-- 1. Draw best CoOrder.
-- 2. Check OrderBook for matchable condition.
-- 3. Match against it.
-- 4. Process MatchResult
-- 5. Go to step#1.
-- Если собрать дополнительную структуру для хранения представления MarketDepth,
-- то можно обойтись без проверок.

module Trading.Exchange.Matching where

import           Control.Monad.RWS.Strict
import           Trading.Exchange.Types

type Base = RWS MatchingEnv [OrderlogRecord] OrderBook


-- | Match 2 orders
match :: Order -> Order -> Base (MatchResult, Trade)
match (Order oid1 p1 v1 s1) (Order oid2 p2 v2 s2) = undefined


-- | Draw best coOrder from OrderBook for given Order.
drawBestCoOrder :: Order -> Base (Maybe Order)
drawBestCoOrder order = undefined


-- | Insert given order into corresponding direction of OrderBook
insertOrder :: Order -> Base ()
insertOrder order = undefined


processMatchResult :: MatchResult -> Base [Trade]
processMatchResult BothMatched = return []


-- | Match recursively
matchRecursively :: Order
                 -> Base [Trade]
matchRecursively order = do
  mbCoOrder <- drawBestCoOrder order
  case mbCoOrder of
    Nothing -> insertOrder order >> return []
    Just coOrder -> do
      (r,t) <- match order coOrder
      rs <- processMatchResult r
      return (t:rs)

