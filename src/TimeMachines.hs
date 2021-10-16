{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TimeMachines where

import           Control.Lens
import           Data.Text    (Text)

type Manufacturer = Text
type Price        = Double
type Year         = Integer

data TimeMachine = TM
                 { _manufacturer :: {-# UNPACK #-} !Manufacturer
                 , _year         :: {-# UNPACK #-} !Year
                 , _price        :: {-# UNPACK #-} !Price
                 } deriving (Eq, Show)

makeLenses ''TimeMachine

setPriceDiscount :: Traversable f => f TimeMachine -> Double -> f TimeMachine
setPriceDiscount tmLst percent = tmLst & traversed . price *~ percent
