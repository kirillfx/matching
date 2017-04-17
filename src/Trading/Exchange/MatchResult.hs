-- |


module Trading.Exchange.MatchResult where

import           Trading.Exchange.Trade


-- | Intermidiate result of matching.
data MatchResult a b
  = AgressorLeft !a Trade
  | CoAgressorLeft !b Trade
  | BothMatched Trade
  | NotMatched !a !b
  deriving (Eq, Show)
