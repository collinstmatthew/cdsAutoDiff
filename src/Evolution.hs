{-# LANGUAGE ScopedTypeVariables #-}

module Evolution(evolveLinear,grid,plotEvolution) where

import Types
import Market(SimpleMarket(..),replaceDates,addMarket,divideMarket,diffMarket,plotPrice,irCurve,hazardRates,plotCurve,getVal',dates,rates,Curve)
import Trades.CashFlow(CashFlows(..))
import Trades.CDS(Credit(..),cdsPrice)

import Numeric.Backprop

import Math(genRange,minimum',maximum')

-- Imports needed for the graph
import qualified Diagrams.Backend.Cairo.CmdLine as CmdInt
import qualified Diagrams.Prelude               as DP
import qualified Diagrams.Backend.Cairo         as CA
import Diagrams.Core.Types(QDiagram)
import Graphics.Rendering.Chart.Grid(wideAbove,aboveN,besideN,above,gridToRenderable,Grid,tspan)
import Graphics.Rendering.Chart.Backend.Diagrams(defaultEnv,runBackendR)
import Graphics.Rendering.Chart.Easy(Renderable,bitmapAlignmentFns)
import Graphics.Rendering.Chart.Layout(layoutToRenderable)
import Graphics.Rendering.Chart.Backend(FillStyle(..))
import Graphics.Rendering.Chart.Easy

import Data.Tuple.Extra

import Data.Time.Calendar(toModifiedJulianDay,Day(ModifiedJulianDay))


import Debug.Trace

type Evolution = [(SimpleMarket,SimpleMarket,Price)]



evolveLinear :: Time -> CashFlows -> Credit -> SimpleMarket -> SimpleMarket -> Int -> Evolution
evolveLinear pdate fl cd mktStart mktEnd n = zip3 allMkts grads' prices where
    intermediateMkt  = divideMarket (fromIntegral (n+1)) $ diffMarket mktEnd mktStart
    allMkts          = take (n+2) $ iterate (addMarket intermediateMkt) mktStart
    prices           = map (evalBP (cdsPrice pdate fl cd)) allMkts
    grads            = map (gradBP (cdsPrice pdate fl cd)) allMkts
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
plotEvolution evolution = do
    -- limits of the rates in the evolution for plotting
    let limits = limitsIrHaz (fst3 (head evolution)) (fst3 (last evolution))
    let tenorLimits = limitsTimeIrHaz (fst3 (head evolution)) (fst3 (last evolution))

    let numPoints = length evolution
        -- first is either time or dummy time
    let prices      = map thd3 evolution
        priceLims   = (minimum prices, maximum prices)
        pricesAccum = map (\a -> take a prices) [1..length evolution]
        result''    = zipWith (\(x,y,z) p2 -> (x,y,p2)   ) evolution pricesAccum

    let fs           = FillStyleSolid (opaque white)
        rendererdImg = map ( (fillBackground fs) . gridToRenderable . (grid priceLims tenorLimits numPoints limits)) result''

    defaultE <- defaultEnv bitmapAlignmentFns 2000 1000

    -- Type annotation is needed to set backend
    let z :: [QDiagram CA.Cairo DP.V2 Double DP.Any] = map (\z-> fst $ runBackendR defaultE z) rendererdImg
    CmdInt.mainWith $ zip z [1..length z]


-- functions for plotting evolutiton
-- # TODO what if the tenors are different for irCurve and hazardRates
-- should probably pass in the pricing date as well
square :: SimpleMarket -> [(Double, Double)]
square mkt = [(x,y) | x <- axis, y <- axis] where
    axis = genRange start end 10
    datesC = view (irCurve . dates) mkt
    start  = fromInteger . toModifiedJulianDay $  head datesC
    end    = fromInteger . toModifiedJulianDay $ last datesC

ef :: Curve -> Curve -> (Double,Double) -> (Double,Double)
ef derivCurve hazardCurve (x,y) = (getVal' derivCurve (ModifiedJulianDay (round y)),getVal' hazardCurve (ModifiedJulianDay (round x)))

vectorField mkt title f = fmap plotVectorField $ liftEC $ do
    c <- takeColor
    plot_vectors_mapf  .= f
    plot_vectors_scale .= 1
    plot_vectors_grid  .= square mkt
    plot_vectors_title .= title
    plot_vectors_style . vector_line_style . line_color .= c
    plot_vectors_style . vector_head_style . point_color .= c

chart mkt = do
        let deri = view irCurve mkt
            heri = view hazardRates mkt
        let start = fromInteger .toModifiedJulianDay $ head $ view dates deri
            end   = fromInteger . toModifiedJulianDay $ last $ view dates deri
        setColors [opaque black, opaque blue]
--        layout_title .= "Derivatives of cds evolution"
        layout_y_axis . laxis_generate  .= scaledAxis def (start,end)
        layout_y_axis . laxis_title     .= "IR Sensitivites"
        layout_x_axis . laxis_generate  .= scaledAxis def (start,end)
        layout_x_axis . laxis_title     .= "Hazard Sensitivites"
        plot $ vectorField mkt "" (ef deri heri)

-- Construct a grid of charts, with a single title accross the top
--grid :: Int -> (SimpleMarket,SimpleMarket,[Price]) -> Grid ( Renderable (LayoutPick Rate Rate Rate))
grid pricelims tenorLimits maxTime limits (mktOrig,mktDeriv,price) = title `wideAbove` (besideN [ vectorP, aboveN [priceP,irP,hzP]])
  where
    (irLimits,hzLimits)  = limits
    irP        = tspan (layoutToRenderable (plotCurve "Interest rates" tenorLimits irLimits mktirCurve)) (1,1)
    hzP        = tspan (layoutToRenderable (plotCurve "Hazard rate" tenorLimits hzLimits mkthzCurve)) (1,1)
    vectorP    = tspan (layoutToRenderable (execEC (chart mktDeriv))) (1,3)
    priceP     = tspan (layoutToRenderable (plotPrice pricelims maxTime price)) (1,1)
    mktirCurve = view irCurve mktOrig
    mkthzCurve = view hazardRates mktOrig
    title      = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre "CDS sensitivies"
    ls         = def { _font_size   = 15 , _font_weight = FontWeightBold }


