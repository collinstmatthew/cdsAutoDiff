{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Trades.CDS(Credit(..),CDS(..),cdsPrice, protectionLegDF,accruedInterest,premiumLeg) where

import Numeric.Backprop
import Control.Lens
import GHC.Generics

import Market(SimpleMarket(..),hazardRates,irCurve,integrateCurve,nodeDates)
import Trades.CashFlow(CashFlows(..),cashDates,quantity,cashFlowValue)
import Types
import Math(difference,differenceR,dot)
import Data.List(zipWith5)
import Data.Sort(uniqueSort)

import Debug.Trace

data Credit = Credit { _notional :: Double,
                       _recoveryRate :: Double} deriving (Show, Generic)

-- credit data
instance Backprop Credit
makeLenses ''Credit


data CDS = CDS { _effective        :: Time,
                 _creditDetails    :: Credit,
                 _premiumLeg       :: CashFlows
               } deriving (Show, Generic)

-- credit data
instance Backprop CDS
makeLenses ''CDS

-- get f_i h_i and b_i from the  curves
getFHB :: Reifies s W =>  BVar s SimpleMarket -> [BVar s Time] -> ([BVar s Rate], [BVar s Rate], [BVar s Rate])
getFHB mkt joinDates = (fi,hi,bi) where
    hCurve    = mkt ^^. hazardRates
    iCurve    = mkt ^^. irCurve

    pDate     = head joinDates

    -- these need to be calculated from i=0 to i = n
    pi        = map (integrateCurve iCurve pDate) joinDates
    qi        = map (integrateCurve hCurve pDate) joinDates
    bi        = zipWith (*) pi qi
    fi        = differenceR Nothing $ map log pi
    hi        = differenceR Nothing $ map log qi

-- computes the discouunt factor of the protecitonLeg
protectionLegDF :: Reifies s W => Time -> Time -> BVar s SimpleMarket -> BVar s Rate
protectionLegDF pDate endDate mkt = sum $ zipWith3 (\f h dB -> -1 * (h / (f+h)) * dB) fi hi diffBi  where
    diffBi     = differenceR Nothing bi
    -- # startDate is actually difference to the pricing date on the cds
    lDates     =  nodeDates pDate endDate mkt

    -- fi and hi should be from i=1 to n
    -- bi should be from i=0 to n
    (fi,hi,bi) =  getFHB mkt lDates

accruedInterest :: Reifies s W => Time -> CashFlows -> BVar s SimpleMarket -> BVar s Price
accruedInterest pDate cf mkt =  (dot cfQuant accrP) where
    -- all the cash flow payment dates
    (cfDates',cfQuant') = unzip $ filter (\x -> fst x > pDate) $ zip (view cashDates cf) (view quantity cf)

    cfDates = sequenceVar $ auto cfDates'
    cfQuant = sequenceVar $ auto cfQuant'

    -- cash flow accrual start and end dates
    cfStartEnd  = zip ((auto pDate) : init cfDates) cfDates

    endDate = last cfDates'
    -- all the mkt not dates joint
    mktDates = nodeDates pDate endDate mkt

    -- all the market nodes in between coupon start and end dates inclusive
    accrNodes = map accrEx cfStartEnd
    accrEx (s,e) = [s] ++ [ i | i <- mktDates, s < i && i < e] ++ [e]

    -- sum over each period then will do product this with eta and multiply by coupon to get result
    accrP    = map (helperF mkt) accrNodes
    -- #TODO implement proper day count convention= here/get

helperF  :: Reifies s W => BVar s SimpleMarket -> [BVar s Time] -> BVar s Price
helperF mkt dates = sum res  where
    (fi,hi,bi) = getFHB mkt dates
    diffBi     = differenceR Nothing bi
    -- proportion of time until next market node date to whole period
    deltaT = map (\x -> x/ diffDays' (last dates) (head dates)) (differenceDayE Nothing dates)

    res        = zipWith5 (\f1 h1 b1 db1 dt ->dt * (h1/(f1+h1)) * (db1/(f1+h1) - b1)) fi hi (tail bi) diffBi deltaT

--cdsPrice :: Reifies s W => Time -> CashFlows -> Credit -> BVar s SimpleMarket -> BVar s Price
cdsPrice :: Reifies s W => Time -> CDS -> BVar s SimpleMarket -> BVar s Price
cdsPrice pDate cds mkt = couponLeg + aI + defaultLeg where
    cashFlows = view premiumLeg cds
    creditData = view creditDetails cds
    effD = max (view effective cds) pDate
    couponLeg = cashFlowValue effD cashFlows mkt
    --take end date to be last coupon payment
    endDate = last $ view cashDates cashFlows

    -- discount from effective date to pricing date
    eDateDiscount = 1.0 / (integrateCurve (mkt ^^. irCurve) (auto pDate) (auto effD))

    aI = accruedInterest effD cashFlows mkt
    defaultLeg = notional' * (1-rr) * protectionLegDF effD endDate mkt where
        notional'  = cD ^^. notional
        rr         = cD ^^. recoveryRate
        cD         = auto creditData
