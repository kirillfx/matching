-- |

{-# LANGUAGE DataKinds #-}

module MatchingSpec where

import           Control.Lens
import           Control.Monad.RWS.Strict        (runRWS)
import           Data.Time
import           Test.Hspec
import           Trading.Exchange.Matching
import           Trading.Exchange.Order
import           Trading.Exchange.OrderBook
import           Trading.Exchange.OrderlogRecord
import           Trading.Exchange.Trade
import           Trading.Exchange.Types


matchingSpec :: Spec
matchingSpec = do
  let t0 = UTCTime (ModifiedJulianDay 57860) 0.2

      e :: UTCTime -> MatchingEnv
      e t = MatchingEnv 0 "NIM_BTC" t 0.0

      b0 :: Order 'BUY
      b0 = Order 0 100.0 10.0 (UTCTime (ModifiedJulianDay 57858) 0.0)

      s0 :: Order 'SELL
      s0 = Order 1 100.0 10.0 (UTCTime (ModifiedJulianDay 57858) 0.1)

      insertB0 = matchOrder (Right b0)
      insertS0 = matchOrder (Left s0)

  describe "Insertion" $ do

    let (_, _, ls1) = runRWS insertB0 (e t0) emptyOrderBook

    it "generates non empty Orderlog for BUY insertion" $ do
      ls1 `shouldNotBe` mempty

    it "OrderlogRecord result" $ do
      head ls1 `shouldBe` (Insertion 0 (getOrderCreationTime b0) 0 BUY 0 100 10)

    let (_,_,ls2) = runRWS insertS0 (e t0) emptyOrderBook

    it "generates non empty Orderlog for SELL insertion" $ do
      ls2 `shouldNotBe` mempty

    it "OrderlogRecord result" $ do
      head ls2 `shouldBe` (Insertion 0 (getOrderCreationTime s0) 0 SELL 0 100 10)

  describe "Matching" $ do

    it "returns matching result of the equally sized orders" $ do

      let prog = mappend <$> matchOrder (Right b0) <*> matchOrder (Left s0)
          (ts,book',ls) = runRWS prog (e t0) emptyOrderBook

      ts `shouldNotBe` mempty

      head ts `shouldBe` (Trade 0 0 100 10 0.0 (getOrderCreationTime s0) SELL)

      book'^.registry `shouldBe` mempty

    it "return matching result of the equally sized orders in opposite order" $ do

      let prog = mappend <$> matchOrder (Left s0) <*> matchOrder (Right b0)
          (ts,book',ls) = runRWS prog (e t0) emptyOrderBook

      ts `shouldNotBe` mempty

      head ts `shouldBe` (Trade 0 0 100 10 0.0 (getOrderCreationTime b0) BUY)

      book'^.registry `shouldBe` mempty
