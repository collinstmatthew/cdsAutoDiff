module Main where

-- bs auto modules
import Market(SimpleMarket(..),Curve(..))
import Trades.CDS(Credit(..),CDS(..))
import Trades.CashFlow(CashFlows(..))
import Types
import Evolution(evolveLinear,plotEvolution)
import Data.Time.Calendar

main :: IO ()
main =  do

    -- set up a list of Dates to use for cash payments etc
    let baseDates = schedule (fromGregorian 2015 9 10) (fromGregorian 2020 10 15) (CalendarDiffDays (-6) 0)

    print $ baseDates

    let n = length baseDates

    -- Create two makets a starting and end market
    let mktStart      = SimpleMarket irCurve  hazardRates where
        irCurve       = Curve baseDates (take n [0.02,0.03..])
        hazardRates   = Curve baseDates (take n [0.01,0.012..])

    let mktEnd        = SimpleMarket irCurve hazardRates where
        irCurve       = Curve baseDates (take n [0.05,0.07..])
        hazardRates   = Curve baseDates (take n [0.066,0.074..])

    let cds = CDS effective creditData fixedLegCashFlow where
        effective        = fromGregorian 2015 10 15
        fixedLegCashFlow = CashFlows baseDates (take n (repeat 0.06))
        creditData       = Credit 5 0.4

    let numPoints   = 30
        pricingDate = fromGregorian 2015 10 20

    --let evolutionEnd = Nothing
    let evolutionEnd = Just $ fromGregorian 2017 10 20

    let evolution = evolveLinear pricingDate cds mktStart mktEnd numPoints evolutionEnd

    plotEvolution $ evolution

    print "finished"
