{-# LANGUAGE ScopedTypeVariables #-}

module Evolution(evolveLinear,plotEvolution) where

import Types
import Market(SimpleMarket(..),replaceDates,addMarket,divideMarket,diffMarket,plotPrice,irCurve,hazardRates,plotCurve,getVal',dates,rates,Curve)
import Trades.CashFlow(CashFlows(..),cashDates)
import Trades.CDS(Credit(..),cdsPrice,CDS(..),premiumLeg)

import Numeric.Backprop

import Math(genRangeDay,minimum',maximum')

-- Imports needed for the graph
import qualified Diagrams.Backend.Cairo.CmdLine as CmdInt
import qualified Diagrams.Prelude               as DP
import qualified Diagrams.Backend.Cairo         as CA
import qualified Diagrams.Core.Types            as DT
import Graphics.Rendering.Chart.Grid(wideAbove,aboveN,besideN,above,gridToRenderable,Grid,tspan)
import Graphics.Rendering.Chart.Backend.Diagrams(defaultEnv,runBackendR)
import Graphics.Rendering.Chart.Easy(Renderable,bitmapAlignmentFns)
import Graphics.Rendering.Chart.Layout(layoutToRenderable)
import Graphics.Rendering.Chart.Backend(FillStyle(..))
import Graphics.Rendering.Chart.Easy

import Graphics.Rendering.Chart.Axis.Time(timeValueFromDouble)

import Data.Tuple.Extra

import Data.Time.Calendar(toModifiedJulianDay,Day(ModifiedJulianDay))

import Debug.Trace

type Evolution = [(SimpleMarket,SimpleMarket,Price)]

evolveLinear :: Time -> CDS -> SimpleMarket -> SimpleMarket -> Integer -> Maybe Time -> ((Time,Time),Evolution)
evolveLinear pdate cds mktStart mktEnd n evEnd = ((pdate,end),zip3 allMkts grads' prices) where
    -- pricing dates
    pDateEvolve :: [Time] = case evEnd of
                              Just e  -> genRangeDay pdate e n
                              Nothing -> take (fromInteger (n+2)) $ repeat pdate

    end = last $view cashDates $ view premiumLeg cds

    intermediateMkt  = divideMarket (fromIntegral (n+1)) $ diffMarket mktEnd mktStart
    allMkts          = take (fromInteger (n+2)) $ iterate (addMarket intermediateMkt) mktStart

    prices           = zipWith (\x y -> evalBP (cdsPrice y cds) x) allMkts pDateEvolve
    grads            = zipWith (\x y -> gradBP (cdsPrice y cds) x) allMkts pDateEvolve

    -- replace the dates in the gradient market with the original dates
    grads'           = zipWith replaceDates allMkts grads

limitsIrHaz :: SimpleMarket -> SimpleMarket -> ((Rate,Rate),(Rate,Rate))
limitsIrHaz mktS mktE = ((minimum' ir, maximum' ir),(minimum' hz, maximum' hz)) where
    ir = view (irCurve . rates) mktS ++ view (irCurve . rates) mktE
    hz = view (hazardRates . rates) mktS ++ view (hazardRates . rates) mktE

limitsTimeIrHaz :: SimpleMarket -> SimpleMarket -> (Time,Time)
limitsTimeIrHaz mktS mktE = (minimum' (ir ++ hz), maximum' (ir ++ hz))  where
    ir = view (irCurve . dates) mktS ++ view (irCurve . dates) mktE
    hz = view (hazardRates . dates) mktS ++ view (hazardRates . dates) mktE

-- plots a linear evolugion of a market
plotEvolution ((start,end), evolution) = do
    -- limits of the rates in the evolution for plotting
    let limits = limitsIrHaz (fst3 (head evolution)) (fst3 (last evolution))
    let tenorLimits = limitsTimeIrHaz (fst3 (head evolution)) (fst3 (last evolution))

    let numPoints = length evolution
        -- first is either time or dummy time
    let prices      = map thd3 evolution
        priceLims   = (minimum prices, maximum prices)
        pricesAccum = map (\a -> take a prices) [1..length evolution]
        result''    = zipWith (\(x,y,z) p2 -> (x,y,p2)   ) evolution pricesAccum

    let renderedRes = map (\x -> (priceRenderable priceLims numPoints x,rateRenderable tenorLimits limits x,vectorRenderable start end x)) result''

    defaultEVec <- defaultEnv bitmapAlignmentFns 2000 2400
    defaultERate <- defaultEnv bitmapAlignmentFns 2000 1600
    defaultEPrice <- defaultEnv bitmapAlignmentFns 2000 800

    -- put all prices into the same list
    let zAll :: [(DT.QDiagram CA.Cairo DP.V2 Double DP.Any,DT.QDiagram CA.Cairo DP.V2 Double DP.Any,DT.QDiagram CA.Cairo DP.V2 Double DP.Any)] = map (\(x,y,z) -> (fst $ runBackendR defaultEPrice x,fst $ runBackendR defaultERate y,fst $ runBackendR defaultEVec z)) renderedRes

    let zTotalAll = map (\(x,y,z) -> z DP.||| (x DP.=== y)) zAll

    --CmdInt.mainWith $ zip zTotal [1..length zTotal]
    CmdInt.mainWith $ zip zTotalAll [1..length zTotalAll]


-- functions for plotting evolutiton
-- # TODO what if the tenors are different for irCurve and hazardRates
-- should probably pass in the pricing date as well

square :: Time -> Time -> SimpleMarket -> [(Time, Time)]
square start end mkt = [(x,y) | x <- axis, y <- axis] where
    -- generate 30 points on the grid
    axis = genRangeDay start end 20

ef :: Curve -> Curve -> (Time,Time) -> (Time,Time)
ef derivCurve hazardCurve (x,y) = (timeValueFromDouble (100* (getVal' derivCurve y)),timeValueFromDouble (100 * (getVal' hazardCurve x)))

vectorField start end mkt title f = fmap plotVectorField $ liftEC $ do
    c <- takeColor
    plot_vectors_mapf  .= f
    plot_vectors_scale .= 1
    plot_vectors_grid  .= square start end mkt
    plot_vectors_title .= title
    plot_vectors_style . vector_line_style . line_color .= c
    plot_vectors_style . vector_head_style . point_color .= c

-- filter the axis so it's only for dates between start and end date don't know why more
-- than that are generated automatically
myaxisvals start end x =  autoTimeValueAxis $ filter (\y -> y >= start && y <= end) x

plotVec start end mkt = do
        let deri = view irCurve mkt
            heri = view hazardRates mkt

        setColors [opaque black, opaque blue]
--        layout_title .= "Derivatives of cds evolution"
        layout_y_axis . laxis_generate  .= myaxisvals start end
        layout_y_axis . laxis_title     .= "IR Sensitivites"
        layout_y_axis . laxis_style . axis_label_style . font_size  .= 36
        layout_y_axis . laxis_title_style . font_size .= 42


        layout_x_axis . laxis_generate  .= myaxisvals start end
        layout_x_axis . laxis_title     .= "Hazard Sensitivites"
        layout_x_axis . laxis_style . axis_label_style . font_size  .= 36
        layout_x_axis . laxis_title_style . font_size .= 42
        plot $ vectorField start end mkt "" (ef deri heri)


rateRenderable :: (Time,Time) -> ((Rate,Rate),(Rate,Rate)) -> (SimpleMarket,b,c) -> Renderable (LayoutPick Time Rate Rate)
rateRenderable tenorLimits limits (mktOrig,mktDeriv,price) = ((fillBackground ( FillStyleSolid (opaque white) )) .  gridToRenderable) (aboveN [irP,hzP])
  where
    (irLimits,hzLimits)  = limits
    irP        = tspan (layoutToRenderable (plotCurve "Foward Interest Rate" tenorLimits irLimits mktirCurve)) (1,1)
    hzP        = tspan (layoutToRenderable (plotCurve "Forward Hazard Rate" tenorLimits hzLimits mkthzCurve)) (1,1)
    mktirCurve = view irCurve mktOrig
    mkthzCurve = view hazardRates mktOrig

priceRenderable :: (Price,Price) -> Int -> (SimpleMarket,b,[Price]) -> Renderable (LayoutPick Double Price Price)
priceRenderable pricelims maxTime (_,_,price) = ((fillBackground ( FillStyleSolid (opaque white) )) .  gridToRenderable) (priceP)
  where
    priceP     = tspan (layoutToRenderable (plotPrice pricelims maxTime price)) (1,1)


vectorRenderable :: Time -> Time -> (SimpleMarket,SimpleMarket,[Price]) -> Renderable (LayoutPick Time Time Time)
vectorRenderable start end (_,mktDeriv,_) = ((fillBackground ( FillStyleSolid (opaque white) )) .  gridToRenderable) vectorP
  where
    vectorP    = tspan (layoutToRenderable (execEC (plotVec start end mktDeriv))) (1,1)
