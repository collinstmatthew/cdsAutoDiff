{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Market(SimpleMarket(..),
              Curve(..),
              dates,
              rates,
              irCurve,
              hazardRates,
              integrateCurve,
              nodeDates,
              getVal,
              yieldCurve) where

import Types

import Control.Lens
import Numeric.Backprop
import GHC.Generics

import Data.Sort(uniqueSort)


import Math(dot,difference)

-- a curve is just time rate points which we can then interpolate however we wish
data Curve = Curve { _dates :: [Time],
                     _rates :: [Rate]
                   } deriving (Show, Generic)

instance Backprop Curve
makeLenses ''Curve

-- Market object is made up of iterest and hazard rates
data SimpleMarket = SimpleMarket { _irCurve     :: Curve,
                                   _hazardRates :: Curve } deriving (Show,Generic)

instance Backprop SimpleMarket
makeLenses ''SimpleMarket

-- take forward rates and gives a discount factor back
-- assums forward rates are piecewise constant
-- careful dot doesn't show error if sizes are difference
integrateCurve :: Reifies s W => BVar s Curve -> BVar s Time -> BVar s Rate
integrateCurve forwardRates t = exp (-(dot timesDiff forwardRates')) where
    forwardRates' = take (length times) (addDummy (sequenceVar (forwardRates ^^. rates) ))
    -- ensure if t is past the last node then add a dummy rate at the end
    addDummy x    = if t > last datesH then x ++ [last x] else x
    datesH        = sequenceVar $ forwardRates ^^. dates
    times         = filteredT ++ [t]
    filteredT     = filter (t >) datesH
    times'        = 0 : init times
    timesDiff     = difference (Just 0) times

nodeDates :: Reifies s W => BVar s SimpleMarket -> [BVar s Time]
nodeDates mkt = uniqueSort (irDates ++ hazDates) where
    hCurve    = mkt ^^. hazardRates
    iCurve    = mkt ^^. irCurve
    hazDates  = sequenceVar $ hCurve ^^. dates
    irDates   = sequenceVar $ iCurve ^^. dates

-- get the appropriate value at the speicif time
getVal :: Reifies s W => BVar s Curve -> BVar s Time -> BVar s Rate
getVal curve time   | numG == 0 = last ratesG
                    | otherwise =  head $ drop numG ratesG where
    numG = length $ filter (time <) datesG
    datesG = sequenceVar (curve ^^. dates)
    ratesG = sequenceVar (curve ^^. rates)

-- gets the yield instead of the discouunt factor
yieldCurve :: Reifies s W => BVar s Curve -> BVar s Time -> BVar s Rate
yieldCurve forwardRates t = (1/t) * log (integrateCurve forwardRates t)

