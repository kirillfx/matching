-- |

module Trading.Exchange.MarketDepth where

import           Data.Map.Strict (Map)
import           Data.Scientific


data MarketDept = MarketDepth { asks :: Map Scientific Scientific
                              , bids :: Map Scientific Scientific
                              } deriving Show
