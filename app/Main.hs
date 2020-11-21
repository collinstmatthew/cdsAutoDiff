{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- bs auto modules
import Market(SimpleMarket(..),Curve(..),irCurve,hazardRates,rates,dates,integrateCurve'')
import Trades.CallOption(callPrice,ModelParams(..))
import Trades.CDS(Credit(..),CDS(..),protectionLegDF,accruedInterest)
import Trades.CashFlow(CashFlows(..),cashFlowValue)

import Types
import Evolution(evolveLinear,plotEvolution)

import Data.Time.Calendar

import Numeric.Backprop

main :: IO ()
main =  do
    let tenorDates = schedule (fromGregorian 2018 9 10) (fromGregorian 2020 10 15) (CalendarDiffDays (-6) 0)

    -- Create out market from curves
    -- These are market forward rates
    let irCurve1      = Curve tenorDates [0.02,0.02,0.02,0.02,0.02]
        hazardRates1  = Curve tenorDates [0.02,0.025,0.03,0.035,0.04]
        mkt1          = SimpleMarket irCurve1 hazardRates1

    let irCurve2      = Curve tenorDates [0.08,0.06,0.06,0.1,0.18]
        hazardRates2  = Curve tenorDates [0.01,0.015,0.02,0.025,0.03]
        mkt2          = SimpleMarket irCurve2 hazardRates2

    -- number of intermediate market points

    -- create cashflows for fixed leg
    -- create credit data notional and recovery rate
    --
    -- # There needs to be a cds effective date so we know where to accrue interest from
    -- current it is just the pricing date
    -- Similarly the termidnation date is just the last coupon date
    --
    let fixedLegCashFlow = CashFlows tenorDates [1,1,1,1,1]
        --creditData       = Credit 10 0.4
        creditData       = Credit 10 0.4
        numPoints = 1

    --let pricingDate = fromGregorian 2018 9 12
    let pricingDate = fromGregorian 2018 10 13
    let effective = fromGregorian 2018 10 14

    let cds = CDS effective creditData fixedLegCashFlow

    print $ tenorDates
    --print $ integrateCurve'' irCurve1 pricingDate (tenorDates!!0)
    --print $ map (integrateCurve'' hazardRates1 pricingDate) tenorDates
--    print $ evalBP (cashFlowValue pricingDate fixedLegCashFlow) mkt1

    plotEvolution $ evolveLinear pricingDate cds mkt1 mkt2 numPoints

    print "finished"
