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
              getVal',
              yieldCurve,
              diffMarket,
              addMarket,
              divideMarket,
              plotCurve,
              plotPrice,
              replaceDates
             ) where

import Types

import Control.Lens
import Numeric.Backprop
import GHC.Generics

import Data.Sort(uniqueSort)
import Math(dot,difference)

import Graphics.Rendering.Chart.Easy

import Debug.Trace

-- a curve is just time rate points which we can then interpolate however we wish
data Curve = Curve { _dates :: [Time],
                     _rates :: [Rate]
                   } deriving (Show, Generic)

instance Backprop Curve
makeLenses ''Curve


nodeDates' :: Curve -> Curve -> [Time]
nodeDates' c1 c2 = uniqueSort (d1 ++ d2) where
    d2    = view dates c1
    d1    = view dates c2


-- get the appropriate value at the speicifc time assuming piecewise constant
getVal' :: Curve -> Time -> Rate
getVal' curve time  | null together = last ratesG
                    | otherwise    = (snd . head) together where
    together = dropWhile (\x -> (fst x) < time)  $ zip datesG ratesG
    datesG = view dates curve
    ratesG = view rates curve


plotCurve :: String -> Curve -> Layout Time Rate
plotCurve name c = execEC $ do
    layout_y_axis . laxis_generate .= scaledAxis def (0,0.1)
    layout_y_axis  . laxis_title  .= name
    layout_x_axis  . laxis_title  .= "Time"
    layout_x_axis . laxis_generate .= scaledAxis def (0,2)
    setColors [opaque black, opaque blue]
    plot $ line "" [  [(s,getVal' c s) | s <- uniqueSort (ss ++ ss')] ]
  where
    eps = 0.001
    ss = view dates c
    -- as we know the curves are constant also plot just before the date
    ss' =  map (\x->x+eps) ss

-- # This is just dummy price currently and isn't got properly
plotPrice maxTime prices = execEC $ do
    layout_y_axis . laxis_generate .= scaledAxis def (11,13)
    layout_x_axis . laxis_generate .= scaledAxis def (0,fromIntegral maxTime)
    layout_x_axis  . laxis_title  .= "Time"
    layout_y_axis  . laxis_title  .= "Price"

    setColors [opaque black, opaque blue]
    plot $ line "" [  [(fromIntegral s, prices!!s ) | s <- [0..length prices - 1]  ] ]


-- find the difference of the two curves
diffCurve :: Curve -> Curve -> Curve
diffCurve c1 c2 = Curve dates1 rates1 where
    dates1 = nodeDates' c1 c2
    rates1 = map (\x -> getVal' c1 x - getVal' c2 x) dates1

-- find the difference of the two curves
addCurve :: Curve -> Curve -> Curve
addCurve c1 c2 = Curve dates1 rates1 where
    dates1 = nodeDates' c1 c2
    rates1 = map (\x -> getVal' c1 x + getVal' c2 x) $ dates1

-- # TODO can make this better using the lenses
divideCurve :: Double -> Curve -> Curve
divideCurve m c1 = Curve (view dates c1) (map (/m) (view rates c1))


-- Market object is made up of iterest and hazard rates
data SimpleMarket = SimpleMarket { _irCurve     :: Curve,
                                   _hazardRates :: Curve } deriving (Show,Generic)

instance Backprop SimpleMarket
makeLenses ''SimpleMarket

diffMarket :: SimpleMarket -> SimpleMarket -> SimpleMarket
diffMarket m1 m2 = SimpleMarket ir hc where
    ir = diffCurve (view irCurve m1) (view irCurve m2)
    hc = diffCurve (view hazardRates m1) (view hazardRates m2)

addMarket :: SimpleMarket -> SimpleMarket -> SimpleMarket
addMarket m1 m2 = SimpleMarket ir hc where
    ir = addCurve (view irCurve m1) (view irCurve m2)
    hc = addCurve (view hazardRates m1) (view hazardRates m2)

divideMarket :: Double -> SimpleMarket -> SimpleMarket
divideMarket m mkt = SimpleMarket ir hc where
    ir = divideCurve m (view irCurve mkt)
    hc = divideCurve m (view hazardRates mkt)

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

-- takes the dates from the first market and puts them into the second market
-- commonly used to put the proper dates into a derivatives market
replaceDates :: SimpleMarket -> SimpleMarket -> SimpleMarket
replaceDates mkt mktDeriv  = mktDeriv'' where
    mktDeriv'  = over (irCurve . dates) (const ratesDates) mktDeriv
    mktDeriv'' = over (hazardRates . dates) (const ratesDates) mktDeriv'

    ratesDates  = view dates . view irCurve $ mkt
    hazardDates = view dates . view hazardRates $ mkt

nodeDates :: Reifies s W => BVar s SimpleMarket -> [BVar s Time]
nodeDates mkt = uniqueSort (irDates ++ hazDates) where
    hCurve    = mkt ^^. hazardRates
    iCurve    = mkt ^^. irCurve
    hazDates  = sequenceVar $ hCurve ^^. dates
    irDates   = sequenceVar $ iCurve ^^. dates

-- get the appropriate value at the speicifc time assuming piecewise constant
getVal :: Reifies s W => BVar s Curve -> BVar s Time -> BVar s Rate
getVal curve time   | null together = last ratesG
                    | otherwise    = (snd . head) together where
    together = dropWhile (\x -> (fst x) < time)  $ zip datesG ratesG
    datesG = sequenceVar (curve ^^. dates)
    ratesG = sequenceVar (curve ^^. rates)

-- gets the yield instead of the discouunt factor
yieldCurve :: Reifies s W => BVar s Curve -> BVar s Time -> BVar s Rate
yieldCurve forwardRates t = (1/t) * log (integrateCurve forwardRates t)
