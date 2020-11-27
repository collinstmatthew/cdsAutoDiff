{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- bs auto modules
import Market(SimpleMarket(..),Curve(..),irCurve,hazardRates,rates,dates,integrateCurve'')
import Trades.CallOption(callPrice,ModelParams(..))
import Trades.CDS(Credit(..),CDS(..),protectionLegDF,accruedInterest,cdsPrice)
import Trades.CashFlow(CashFlows(..),cashFlowValue)
import Types
import Evolution(evolveLinear,plotEvolution)

import Data.Time.Calendar
import Numeric.Backprop
import Data.Tuple.Extra

main :: IO ()
main =  do

    -- set up a list of Dates to use for cash payments etc
    let baseDates = schedule (fromGregorian 2018 9 10) (fromGregorian 2020 10 15) (CalendarDiffDays (-6) 0)

    print $ baseDates

    -- Create two makets a starting and end market
    let mktStart      = SimpleMarket irCurve  hazardRates where
        irCurve       = Curve baseDates [0.02,0.025,0.035,0.05,0.07]
        hazardRates   = Curve baseDates [0.01,0.012,0.014,0.016,0.018]

    let mktEnd        = SimpleMarket irCurve hazardRates where
        irCurve       = Curve baseDates [0.01,0.01,0.01,0.01,0.01]
        hazardRates   = Curve baseDates [0.066,0.074,0.077,0.081,0.087]

    let cds = CDS effective creditData fixedLegCashFlow where
        effective        = fromGregorian 2018 10 14
        fixedLegCashFlow = CashFlows baseDates [0.06,0.06,0.06,0.06,0.06]
        creditData       = Credit 5 0.4

    let numPoints   = 30
        pricingDate = fromGregorian 2018 11 14

    let evolutionEnd = Nothing

--    print $ evalBP (cdsPrice (fromGregorian 2020 04 16) cds) mktStart

    let evolution = evolveLinear pricingDate cds mktStart mktEnd numPoints evolutionEnd

    plotEvolution $ evolution

    print "finished"
