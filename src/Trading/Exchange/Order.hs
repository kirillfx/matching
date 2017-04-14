-- |

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}


module Trading.Exchange.Order where

import           Control.Arrow          ((&&&))
import           Control.Lens
import           Data.Function          (on)
import           Data.Time
import           GHC.Generics
import           Trading.Exchange.Types

data Order (d :: Side) =
  Order { _orderId           :: !OrderId
        , _orderPrice        :: !Price
        , _orderSize         :: !Size
        , _orderCreationTime :: !UTCTime
        } deriving (Generic, Show)


makeLenses ''Order


instance Eq (Order d) where
  (==) = (==) `on` _orderId


instance Ord (Order d) where
  compare = compare `on` (_orderPrice &&& _orderId)
