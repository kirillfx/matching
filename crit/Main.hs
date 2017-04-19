-- |

{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Monad                   (forever, mapM)
import           Control.Monad.Random.Strict
import           Control.Monad.RWS.Strict
import           Criterion.Main
import           Data.Set                        (Set)
import qualified Data.Set                        as S
import           Data.Time
import           Trading.Exchange.Matching
import           Trading.Exchange.Order
import           Trading.Exchange.OrderBook
import           Trading.Exchange.OrderlogRecord
import           Trading.Exchange.Trade
import           Trading.Exchange.Types


randomOrderM :: RandomGen g
           => (Double,Double)
           -> (Double,Double)
           -> OrderId
           -> RandT g IO (Order d)
randomOrderM (pmin,pmax) (smin,smax) oid = do
  t <- liftIO getCurrentTime
  p <- realToFrac <$> getRandomR (pmin,pmax)
  s <- realToFrac <$> getRandomR (smin,smax)
  return $ Order oid p s t


randomOrderMs :: RandomGen g
            => (Double,Double)
            -> (Double,Double)
            -> Integer
            -> RandT g IO [Order d]
randomOrderMs ps ss n = mapM (randomOrderM ps ss) [0..n]


templateBuy :: RandomGen g => Integer -> RandT g IO [Order 'BUY]
templateBuy = randomOrderMs (100,1000) (1,20)


templateSell :: RandomGen g => Integer -> RandT g IO [Order 'SELL]
templateSell = randomOrderMs (100,1000) (1,20)


main :: IO ()
main = do
  buys10 <- evalRandTIO (templateBuy 10) :: IO [Order 'BUY]
  buys100 <- evalRandTIO (templateBuy 100) :: IO [Order 'BUY]
  buys1000 <- evalRandTIO (templateBuy 1000) :: IO [Order 'BUY]
  buys10000 <- evalRandTIO (templateBuy 10000) :: IO [Order 'BUY]

--  print [buys10, buys100, buys]

  -- sells10 <- take 10 <$> evalRandTIO templateSell :: IO [Order 'SELL]
  -- sells100 <- take 100 <$> evalRandTIO templateSell :: IO [Order 'SELL]
  -- sells1000 <- take 1000 <$> evalRandTIO templateSell :: IO [Order 'SELL]
  -- sells10000 <- take 10000 <$> evalRandTIO templateSell :: IO [Order 'SELL]

  t <- getCurrentTime
  let e = MatchingEnv 0 "BTC_NIM" t 0.0
      b = Order 10001 500 100 t :: Order 'BUY
      s = Order 10001 500 100 t :: Order 'SELL
      book ss bs = OrderBook (S.fromList ss) (S.fromList bs) mempty 0 0
      matchCase :: [Order 'BUY]
                -> Order 'SELL
                -> ([Trade], OrderBook, [OrderlogRecord])
      matchCase bs x = runRWS (matchRecursively x) e (book mempty bs)

  defaultMain [ bgroup "buyInsertion"
                [ bench "10" $ whnf (flip insertBuy (book mempty buys10)) b
                , bench "100" $ whnf (flip insertBuy (book mempty buys100)) b
                , bench "1000" $ whnf (flip insertBuy (book mempty buys1000)) b
                , bench "10000" $ whnf (flip insertBuy (book mempty buys10000)) b
                ]
              , bgroup "matching" [ bench "10" $ whnf (matchCase buys10) s
                                  , bench "100" $ whnf (matchCase buys100) s
                                  , bench "1000" $ whnf (matchCase buys1000) s
                                  , bench "10000" $ whnf (matchCase buys10000) s
                                  ]
              -- , bgroup "sellInsertion" [

              --   ]
              ]
