{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Trades.CDS(Credit(..),cdsPrice) where

import Numeric.Backprop
import Control.Lens
import GHC.Generics

import Market(SimpleMarket(..),hazardRates,irCurve,integrateCurve,nodeDates)
import Trades.CashFlow(CashFlows(..),cashDates,quantity,cashFlowValue)
import Types
import Math(difference,differenceR,dot)
import Data.List(zipWith5)

data Credit = Credit { _notional :: Double,
                      _recoveryRate :: Double} deriving (Show, Generic)

-- credit data
instance Backprop Credit
makeLenses ''Credit

-- get f_i h_i and b_i from the  curves
getFHB :: Reifies s W =>  BVar s SimpleMarket -> [BVar s Time] -> ([BVar s Rate], [BVar s Rate], [BVar s Rate])
getFHB mkt joinDates = (fi,hi,bi) where
    hCurve    = mkt ^^. hazardRates
    iCurve    = mkt ^^. irCurve
    pi        = map (integrateCurve iCurve) joinDates
    qi        = map (integrateCurve hCurve) joinDates
    bi        = zipWith (*) pi qi
    fi        = differenceR Nothing $ map log pi
    hi        = differenceR Nothing $ map log qi

-- computes the discouunt factor of the protecitonLeg
--protectionLegDF :: Reifies s W => BVar s Market -> BVBar s Rate
protectionLegDF :: Reifies s W => BVar s SimpleMarket -> BVar s Rate
protectionLegDF mkt = sum $ zipWith3 (\f h dB -> (h / (f+h)) * dB) fi hi diffBi  where
    diffBi    = differenceR (Just 1) bi
    joinDates = 0 : nodeDates mkt
    (fi,hi,bi) = getFHB mkt joinDates

accruedInterest :: Reifies s W => CashFlows -> BVar s SimpleMarket -> BVar s Price
accruedInterest cf mkt = dot quantityCashFlows  (zipWith (*) eta accrP) where
    -- all the cash flow payment dates
    cfDates  = sequenceVar $ auto cf ^^. cashDates
    -- cash flow start and end dates
    cfStartEnd  = zip (0 : init cfDates) cfDates

    -- all the mkt not dates joint
    mktDates = nodeDates mkt

    -- all the market nodes in between coupon start and end dates inclusive
    accrNodes = map accrEx cfStartEnd
    accrEx (s,e) = [s] ++ [ i | i <- mktDates, s < i && i < e] ++ [e]

    -- sum over each period then will do product this with eta and multiply by coupon to get result
    accrP    = map (helperF mkt) accrNodes
    -- #TODO what is the difference between eta and Delta_i
    eta      = difference (Just 0) cfDates
    quantityCashFlows = sequenceVar $ auto cf ^^. quantity

helperF  :: Reifies s W => BVar s SimpleMarket -> [BVar s Rate] -> BVar s Price
helperF mkt dates = sum res  where
    (fi,hi,bi) = getFHB mkt dates
    diffBi     = differenceR Nothing bi
    dti        = difference Nothing dates
    res        = zipWith5 (\f1 h1 b1 db1 dt1 -> (dt1 * h1)/(f1+h1) * (db1/(f1+h1) - b1)) fi hi bi diffBi dti

cdsPrice :: Reifies s W => CashFlows -> Credit -> BVar s SimpleMarket -> BVar s Price
cdsPrice cashFlows creditData mkt = couponLeg - aI  - defaultLeg where
    couponLeg = cashFlowValue cashFlows mkt
    aI = accruedInterest cashFlows mkt
    defaultLeg = notional' * (1-rr) * protectionLegDF mkt where
        notional'  = cD ^^. notional
        rr   = cD ^^. recoveryRate
        cD   = auto creditData


