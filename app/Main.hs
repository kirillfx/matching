-- |

{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Monad.RWS.Strict        (runRWS)
import           Data.Time
import           System.Exit                     (exitSuccess)
import           Trading.Exchange.MarketDepth
import           Trading.Exchange.Matching
import           Trading.Exchange.Order
import           Trading.Exchange.OrderBook
import           Trading.Exchange.OrderlogRecord
import           Trading.Exchange.Types


e :: UTCTime -> MatchingEnv
e t = MatchingEnv 0 "NIM_BTC" t 0.0


b0 :: Order 'BUY
b0 = Order 0 100.0 10.0 (UTCTime (ModifiedJulianDay 57858) 0.0)


s0 :: Order 'SELL
s0 = Order 1 100.0 10.0 (UTCTime (ModifiedJulianDay 57858) 0.1)


prog :: Base [Trade]
prog = do
  matchOrder (Right b0)
  matchOrder (Left s0)

main :: IO ()
main = do
  t <- getCurrentTime
  let (ts, book, ls) = runRWS prog (e t) emptyOrderBook
  mapM_ print ls
  mapM_ print ts
  print book
