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




import Graphics.Rendering.Chart.Grid(wideAbove,aboveN,besideN,above,gridToRenderable,Grid,tspan)
import Graphics.Rendering.Chart.Backend.Diagrams(defaultEnv,runBackendR)
import Graphics.Rendering.Chart.Easy(Renderable,bitmapAlignmentFns)
import Graphics.Rendering.Chart.Layout(layoutToRenderable)
import Graphics.Rendering.Chart.Backend(FillStyle(..))
import Graphics.Rendering.Chart.Easy
import Market(SimpleMarket(..),replaceDates,addMarket,divideMarket,diffMarket,plotPrice,irCurve,hazardRates,plotCurve,getVal',dates,rates,Curve)
import Data.Sort(uniqueSort)
import Graphics.Rendering.Chart.Axis.Time
import Data.Time.Clock
import qualified Data.Time.Format as DF

plotTime :: String -> Curve -> Layout Double Rate
plotTime name c = execEC $ do
    layout_y_axis . laxis_generate .= scaledAxis def (0,0.02)
    layout_y_axis . laxis_title    .= name
    layout_x_axis . laxis_title    .= "Time"
    layout_x_axis . laxis_generate .= scaledAxis def timeLimits
    setColors [opaque black, opaque blue]
    plot $ line "" [  [(fromIntegral (toModifiedJulianDay s),getVal' c s) | s <- uniqueSort (ss ++ ss')] ]
  where
    eps::Integer = 1
    ss = view dates c
    -- as we know the curves are constant also plot just before the date
    ss' =  map (addDays eps) ss
    timeLimits = ((fromIntegral .toModifiedJulianDay) (fromGregorian 2018 9 10) , (fromIntegral .toModifiedJulianDay) (fromGregorian 2020 10 15) )


plotTime2 :: String -> Curve -> Layout Time Rate
plotTime2 name c = execEC $ do
    layout_y_axis . laxis_generate .= scaledAxis def (0,0.02)
    layout_y_axis . laxis_title    .= name
    layout_x_axis . laxis_title    .= "Time"
    layout_x_axis . laxis_generate .= myAxisFun
    setColors [opaque black, opaque blue]
    plot $ line "" [  [(s,getVal' c s) | s <- uniqueSort (ss ++ ss')] ]
  where
    eps::Integer = 1
    ss = view dates c
    -- as we know the curves are constant also plot just before the date
    ss' =  map (addDays eps) ss
    timeLimits = (fromGregorian 2018 9 10 , fromGregorian 2020 10 15)

myAxisFun :: TimeValue t => AxisFn t
myAxisFun pts = timeValueAxis months months (ft "%b") BetweenTicks years  (ft "%Y") BetweenTicks pts  where
    ft    = DF.formatTime DF.defaultTimeLocale


main :: IO ()
main =  do

    -- set up a list of Dates to use for cash payments etc
    let baseDates = schedule (fromGregorian 2018 9 10) (fromGregorian 2020 10 15) (CalendarDiffDays (-6) 0)

    -- Create two makets a starting and end market
    let mktStart      = SimpleMarket irCurve  hazardRates where
        irCurve       = Curve baseDates [0.02,0.02,0.02,0.02,0.02]
        hazardRates   = Curve baseDates [0.02,0.025,0.03,0.035,0.04]

    let mktEnd        = SimpleMarket irCurve hazardRates where
        irCurve       = Curve baseDates [0.05,0.06,0.06,0.1,0.18]
        hazardRates   = Curve baseDates [0.01,0.015,0.02,0.025,0.03]

    let cds = CDS effective creditData fixedLegCashFlow where
        effective        = fromGregorian 2018 10 14
        fixedLegCashFlow = CashFlows baseDates [1,1,1,1,1]
        creditData       = Credit 10 0.4

    let numPoints   = 20
        pricingDate = fromGregorian 2018 10 13

    let hzP  = tspan (layoutToRenderable (plotTime2 "Hazard rate" irCurve)) (1,1)


--    plotEvolution $ evolveLinear pricingDate cds mktStart mktEnd numPoints

    print "finished"
