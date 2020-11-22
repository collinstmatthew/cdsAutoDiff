{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Types(Rate,Price,Vol,Time,dateFrac,DayConvention(..),differenceDay,differenceDay',schedule,diffDays', differenceDayE) where

import Data.Time.Calendar
import GHC.Generics
import Control.Lens
import Numeric.Backprop

-- Setup some dates for model
type Rate  = Double
type Price = Double
type Vol   = Double
type Time  = Day

-- day count convention
data DayConvention = ACT365F

deriving instance Generic Day

instance Backprop Day


-- generate schedule from start date end date and calendar diff
schedule :: Day -> Day -> CalendarDiffDays -> [Day]
schedule start end diff = reverse $ takeWhile (> start) $ iterate (addGregorianDurationClip diff) end

--dateFrac :: Day -> Day -> DayConvention -> Double
dateFrac :: Reifies s W => BVar s Day -> BVar s  Day -> DayConvention -> BVar s Double
dateFrac d1 d2 ACT365F = (diffDays' d1 d2) / 365

-- #TODO I'm not sure this is the best way to do things currently with this manual defintion but
-- it seems to compile ago so meh
diffDays' :: (Reifies s W)
    => BVar s Day
    -> BVar s Day
    -> BVar s Double
diffDays' = liftOp2 . op2 $ \x1 x2 ->  (fromInteger (diffDays x1 x2), \dzdy -> (ModifiedJulianDay (round dzdy),ModifiedJulianDay (round dzdy)) )

differenceDay :: Reifies s W => Maybe (BVar s Day) -> DayConvention ->  [BVar s Day] -> [BVar s Double]
differenceDay (Just begin) dc l = zipWith (\x y -> dateFrac x y dc) l (begin : init l)
differenceDay Nothing dc l = zipWith (\x y -> dateFrac x y dc) (tail l) (init l)

differenceDay' :: Maybe Day -> DayConvention ->  [Day] -> [Double]
differenceDay' (Just begin) dc l = zipWith (\x y -> (fromIntegral (diffDays x y))/365) l (begin : init l)
differenceDay' Nothing dc l = zipWith (\x y -> (fromIntegral (diffDays x y))/365) (tail l) (init l)


differenceDayE :: Reifies s W => Maybe (BVar s Day) ->  [BVar s Day] -> [BVar s Double]
differenceDayE (Just begin) l = zipWith (\x y -> diffDays' x y) l (begin : init l)
differenceDayE Nothing l = zipWith (\x y -> diffDays' x y) (tail l) (init l)
