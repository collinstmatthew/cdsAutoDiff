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


main :: IO ()
main =  do

    -- set up a list of Dates to use for cash payments etc
    let baseDates = schedule (fromGregorian 2018 9 10) (fromGregorian 2020 10 15) (CalendarDiffDays (-6) 0)

    print $ baseDates

    -- Create two makets a starting and end market
    let mktStart      = SimpleMarket irCurve  hazardRates where
        irCurve       = Curve baseDates [0.02,0.025,0.03,0.035,0.04]
        hazardRates   = Curve baseDates [0.001,0.002,0.002,0.003,0.004]

    let mktEnd        = SimpleMarket irCurve hazardRates where
        irCurve       = Curve baseDates [0.02,0.025,0.025,0.025,0.025]
        hazardRates   = Curve baseDates [0.003,0.003,0.003,0.003,0.003]

    let cds = CDS effective creditData fixedLegCashFlow where
        effective        = fromGregorian 2018 10 14
        fixedLegCashFlow = CashFlows baseDates [0.3,0.3,0.3,0.3]
        creditData       = Credit 5 0.4

    let numPoints   = 25
        pricingDate = fromGregorian 2018 09 13

    --let evolutionEnd = Just $ fromGregorian 2018 10 20
--    let evolutionEnd = Just $ fromGregorian 2020 10 15
    let evolutionEnd = Nothing

    print $ evalBP (cdsPrice pricingDate cds) mktStart

--    plotEvolution $ evolveLinear pricingDate cds mktStart mktEnd numPoints evolutionEnd

    print "finished"
