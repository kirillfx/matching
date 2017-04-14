-- | Matching algorithm.

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Trading.Exchange.Matching where

import           Control.Monad              (void)
import           Control.Monad.RWS.Strict
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Trading.Exchange.Order
import           Trading.Exchange.OrderBook
import           Trading.Exchange.Types

type Base = RWS MatchingEnv [OrderlogRecord] OrderBook


-- | Delete Order by OrderId
-- cancelOrder :: OrderId -> Base (Either String ())
-- cancelOrder oid = do
--   (side,price) <- gets registry
--   void $ modify (cancelOrder' oid)
--   where
--     cancelOrder' :: OrderId -> Side -> Price -> OrderBook -> OrderBook
--     cancelOrder' i (OrderBook a b) = OrderBook a' b'
--       where
--         a' = M.filter


class Matching a b | a -> b, b -> a where

  drawBestCoOrder :: Base (Maybe b)

  match :: a -> b -> Base (MatchResult a b, Trade)

  insertOrderM :: a -> Base ()

  processMatchResult :: MatchResult a b -> Base [Trade]

  matchRecursively :: a -> Base [Trade]

  matchRecursively order = do
    mbCoOrder <- drawBestCoOrder
    case mbCoOrder of
      Nothing -> insertOrderM order >> return []
      Just coOrder -> do
        (r,t) <- match order coOrder
        rs <- processMatchResult r
        return (t:rs)


instance Matching (Order 'BUY) (Order 'SELL) where
  drawBestCoOrder = undefined
  match buy sell = undefined
  insertOrderM buy = undefined
  processMatchResult mr = undefined


instance Matching (Order 'SELL) (Order 'BUY) where
  drawBestCoOrder = undefined
  match buy sell = undefined
  insertOrderM buy = undefined
  processMatchResult mr = undefined


-- | Top level function
matchOrder :: (Matching a b, Matching b a) => Either a b -> Base [Trade]
matchOrder e = case e of
  Left sell -> matchRecursively sell
  Right buy -> matchRecursively buy
