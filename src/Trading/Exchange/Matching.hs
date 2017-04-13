-- | Matching algorithm.

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Trading.Exchange.Matching where

import           Control.Monad.RWS.Strict
import           Trading.Exchange.Types


type Base = RWS MatchingEnv [OrderlogRecord] OrderBook


-- | Delete Order by OrderId
cancelOrder :: OrderId -> Base (Either String ())
cancelOrder oid = undefined


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
matchOrder :: Either Sell Buy -> Base [Trade]
matchOrder e = case e of
  Left sell -> matchRecursively sell
  Right buy -> matchRecursively buy
