-- | Types and structures deifinitions.

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}

module Trading.Exchange.Types where

import           Data.Function   (on)
import           Data.Scientific
import           Data.Time
import           GHC.Generics


type OrderId = Integer
type Price = Scientific
type Size = Scientific
type TradeId = Integer
type ProductId = Integer
type Fee = Scientific


-- | Environment type for storing all static information about product service.
-- Timestamp fixed here because of latency between FrontendService and
-- Product service.
data MatchingEnv =
  MatchingEnv { productId     :: !ProductId
              , productName   :: !String
              , executionTime :: !UTCTime -- ^ Fixed execution time.
              , fee           :: Fee
              } deriving Show


instance Eq MatchingEnv where
  (==) = (==) `on` productId


-- | Direction of Order, Trade.
data Side = BUY | SELL deriving (Generic, Show, Eq)
