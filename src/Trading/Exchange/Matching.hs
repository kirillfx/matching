-- | Matching algorithm.
-- Generalized:
-- 1. Draw best CoOrder.
-- 2. Check OrderBook for matchable condition.
-- 3. Match against it.
-- 4. Process MatchResult
-- 5. Go to step#1.
-- Если собрать дополнительную структуру для хранения представления MarketDepth,
-- то можно обойтись без проверок.

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Trading.Exchange.Matching where

import           Control.Monad.RWS.Strict
import           Trading.Exchange.Types


type Base = RWS MatchingEnv [OrderlogRecord] OrderBook


class (Order a, Order b) => Matching a b | a -> b, b -> a where

  drawBestCoOrder :: Base (Maybe b)

  match :: a -> b -> Base (MatchResult a b, Trade)

  insertOrder :: a -> Base ()

  processMatchResult :: MatchResult a b -> Base [Trade]

  matchRecursively :: a -> Base [Trade]

  matchRecursively order = do
    mbCoOrder <- drawBestCoOrder
    case mbCoOrder of
      Nothing -> insertOrder order >> return []
      Just coOrder -> do
        (r,t) <- match order coOrder
        rs <- processMatchResult r
        return (t:rs)


instance Matching Buy Sell where
  drawBestCoOrder = undefined
  match buy sell = undefined
  insertOrder buy = undefined
  processMatchResult mr = undefined


instance Matching Sell Buy where
  drawBestCoOrder = undefined
  match sell buy = undefined
  insertOrder sell = undefined
  processMatchResult mr = undefined


-- | Top level function
matchOrder :: Either Buy Sell -> Base [Trade]
matchOrder e = case e of
  Left buy   -> matchRecursively buy
  Right sell -> matchRecursively sell
