-- |

module Trading.Exchange.OrderlogRecord where

import           Data.Function          (on)
import           Data.Time
import           Trading.Exchange.Types


-- | Order execution log.
-- NO,SECCODE,BUYSELL,TIME,ORDERNO,ACTION,PRICE,VOLUME,TRADENO,TRADEPRICE
data OrderlogRecord
  = Insertion { orderlogID        :: Integer
              , orderlogTimestamp :: UTCTime
              , orderlogProductId :: ProductId
              , orderlogSide      :: Side
              , orderlogOrderID   :: OrderId
              , orderlogPrice     :: Price
              , orderlogSize      :: Size
              }
  | Deletion { orderlogID        :: Integer
             , orderlogTimestamp :: UTCTime
             , orderlogProductId :: ProductId
             , orderlogSide      :: Side
             , orderlogOrderID   :: OrderId
             , orderlogPrice     :: Price
             , orderlogSize      :: Size
             }
  | Execution { orderlogID         :: Integer
              , orderlogTimestamp  :: UTCTime
              , orderlogProductId  :: ProductId
              , orderlogOrderID    :: OrderId
              , orderlogSide       :: Side
              , orderlogPrice      :: Price
              , orderlogSize       :: Size
              , orderlogTradeID    :: TradeId
              , orderlogTradePrice :: Price
              }
  deriving (Show)


instance Eq OrderlogRecord where
  (==) = (==) `on` orderlogID


instance Ord OrderlogRecord where
  compare = compare `on` orderlogID
