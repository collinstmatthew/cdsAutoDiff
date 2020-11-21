{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- bs auto modules
import Market(SimpleMarket(..),Curve(..),irCurve,hazardRates,rates,dates,integrateCurve'')
import Trades.CallOption(callPrice,ModelParams(..))
import Trades.CDS(Credit(..),protectionLegDF,accruedInterest)
import Trades.CashFlow(CashFlows(..))

import Types
import Evolution(evolveLinear,plotEvolution)

import Data.Time.Calendar

import Numeric.Backprop

main :: IO ()
main =  do

    -- Bs call price test
    --let bs = MP { _r = 0.03, _strike = 50, _sigma = 1, _currentTime = 0.0, _endTime = 1.0, _st = 40.0}
    --print $ gradBP callPrice bs

    let tenorDates = schedule (fromGregorian 2018 9 10) (fromGregorian 2020 10 15) (CalendarDiffDays (-6) 0)

    -- Create out market from curves
    -- These are market forward rates
    let irCurve1      = Curve tenorDates [0.02,0.02,0.02,0.02,0.02]
        hazardRates1  = Curve tenorDates [0.02,0.025,0.03,0.035,0.04]
        mkt1          = SimpleMarket irCurve1 hazardRates1

    let irCurve2      = Curve tenorDates [0.02,0.02,0.02,0.02,0.02]
        hazardRates2  = Curve tenorDates [0.02,0.025,0.03,0.035,0.04]
        mkt2          = SimpleMarket irCurve2 hazardRates2

    -- number of intermediate market points

    -- create cashflows for fixed leg
    -- create credit data notional and recovery rate
    --
    -- # There needs to be a cds effective date so we know where to accrue interest from
    -- current it is just the pricing date
    -- Similarly the termidnation date is just the last coupon date
    --
    let fixedLegCashFlow = CashFlows tenorDates [0.1,0.1,0.1,0.1,0.1]
        --creditData       = Credit 10 0.4
        creditData       = Credit 10 0.4
        numPoints = 1

    let pricingDate = fromGregorian 2018 9 12

    --print $ tenorDates
    --print $ map (integrateCurve'' irCurve1 pricingDate) tenorDates
    --print $ integrateCurve'' irCurve1 pricingDate pricingDate

    --print $ evalBP (accruedInterest pricingDate fixedLegCashFlow) mkt1

    plotEvolution $ evolveLinear pricingDate fixedLegCashFlow creditData mkt1 mkt2 numPoints



--    print $ evalBP2 integrateCurve hazardRates 3
--    print $ result
--    print $ evolveLinear fixedLegCashFlow creditData mkt
    print "finished"
