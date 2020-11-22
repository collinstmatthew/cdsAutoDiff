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

    -- set up a list of Dates to use for cash payments etc
    let baseDates = schedule (fromGregorian 2018 9 10) (fromGregorian 2020 10 15) (CalendarDiffDays (-6) 0)

    -- Create two makets a starting and end market
    let mktStart      = SimpleMarket irCurve  hazardRates where
        irCurve       = Curve baseDates [0.02,0.02,0.02,0.02,0.02]
        hazardRates   = Curve baseDates [0.02,0.025,0.03,0.035,0.04]

    let mktEnd        = SimpleMarket irCurve hazardRates where
        irCurve       = Curve baseDates [0.2,0.2,0.2,0.2,0.2]
        hazardRates   = Curve baseDates [0.01,0.015,0.02,0.025,0.03]

    let cds = CDS effective creditData fixedLegCashFlow where
        effective        = fromGregorian 2018 10 14
        fixedLegCashFlow = CashFlows baseDates [1,1,1,1,1]
        creditData       = Credit 10 0.4

    let numPoints   = 1
        pricingDate = fromGregorian 2018 10 13

    plotEvolution $ evolveLinear pricingDate cds mktStart mktEnd numPoints

    print "finished"
