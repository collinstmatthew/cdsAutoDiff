{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- bs auto modules
import Market(SimpleMarket(..),Curve(..),diffMarket,addMarket,divideMarket,plotPrice,plotCurve,irCurve,hazardRates,rates,getVal',dates)
import Trades.CallOption(callPrice,ModelParams(..))
import Trades.CDS(Credit(..),cdsPrice)
import Trades.CashFlow(CashFlows(..))
import Data.List(iterate)

import Types
import Debug.Trace
import Data.Tuple.Extra
import Numeric.Backprop

import Graphics.Gnuplot.Simple
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Monad.IO.Class

import qualified Diagrams.Backend.Cairo as CA
import Diagrams.Core.Types(QDiagram)

import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv,  runBackendR)
import  Graphics.Rendering.Chart.Easy (Renderable,bitmapAlignmentFns)
import Diagrams.TwoD.Size(mkSizeSpec2D)

import qualified  Diagrams.Backend.Cairo.Internal as Int
import qualified  Diagrams.Backend.Cairo.CmdLine as CmdInt
import qualified  Diagrams.Prelude as DP

import Graphics.Rendering.Chart.Grid(wideAbove,aboveN,besideN,above,gridToRenderable,Grid,aboveWide,wideAbove,tspan,tval,Span)

import Graphics.Rendering.Chart.Layout(layoutToRenderable)

import Graphics.Rendering.Chart.Backend(FillStyle(..))

r' :: Double -> Double -> Double -> Double
r' x y z = sqrt $ x^2 + y^2 + z^2

-- square for the graph
square :: [(Rate, Rate)]
square = [(x,y) | x <- axis, y <- axis] where
    axis = [ 0.1*x' | x' <- [1..20]]

-- not currently dependent on y
--ef derivCurve hazardCurve (x,y) = ((getVal' derivCurve x)/r,(getVal' hazardCurve y)/r) where r = r' x y 100
ef derivCurve hazardCurve (x,y) = ((getVal' derivCurve y)/r,(getVal' hazardCurve x)/r) where r = 1.0

vectorField title f = fmap plotVectorField $ liftEC $ do
    c <- takeColor
    plot_vectors_mapf  .= f
    plot_vectors_scale .= 1
    plot_vectors_grid  .= square
    plot_vectors_style . vector_line_style . line_color .= c
    plot_vectors_style . vector_head_style . point_color .= c
    plot_vectors_title .= title

chart deri heri =  do
        setColors [opaque black, opaque blue]

--        layout_title .= "Derivatives of cds evolution"
        layout_y_axis  . laxis_generate  .= scaledAxis def (0,2)
        layout_y_axis  . laxis_title  .= "IR Sensitivites"
        layout_x_axis  . laxis_generate .= scaledAxis def (0,2)
        layout_x_axis  . laxis_title  .= "Hazard Sensitivites"

        plot $ vectorField "" (ef deri heri)

-- Construct a grid of charts, with a single title accross the top
grid :: Int -> (SimpleMarket,SimpleMarket,[Price]) -> Grid ( Renderable (LayoutPick Rate Rate Rate))
--grid maxTime (mktOrig,mktDeriv,price) = title `wideAbove` above  (besideN [ vectorP,irP, hzP]) priceP
grid maxTime (mktOrig,mktDeriv,price) = title `wideAbove` (besideN [ vectorP, aboveN [priceP,irP,hzP]])
--grid maxTime (mktOrig,mktDeriv,price) = title `wideAbove` (above irP hzP)
  where
    --irP = layoutToGrid (plotCurve "Interest rates" mktirCurve)
    --hzP = layoutToGrid (plotCurve "Hazard rate" mkthzCurve)
    irP = tspan (layoutToRenderable (plotCurve "Interest rates" mktirCurve)) (1,1)
    hzP = tspan (layoutToRenderable (plotCurve "Hazard rate" mkthzCurve)) (1,1)
    --vectorP    = layoutToGrid (pltVectorMkt 1 1 mktDeriv)
    vectorP    = tspan (layoutToRenderable (pltVectorMkt mktDeriv)) ((1,3))
    priceP     = tspan (layoutToRenderable (plotPrice maxTime price)) (1,1)
--    priceP     = layoutToGrid (plotPrice maxTime price)
    mktirCurve = view irCurve mktOrig
    mkthzCurve = view hazardRates mktOrig
    title = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre "CDS sensitivies"
    ls = def { _font_size   = 15 , _font_weight = FontWeightBold }


evolveLinear :: CashFlows -> Credit -> SimpleMarket -> SimpleMarket -> Int -> [(SimpleMarket,SimpleMarket,Price)]
evolveLinear fl cd mktStart mktEnd n = zip3 allMkts grads prices where
    intermediateMkt  = divideMarket (fromIntegral n) $ diffMarket mktEnd mktStart
    allMkts          = take (n+2) $ iterate (addMarket intermediateMkt) mktStart
    prices           = map (evalBP (cdsPrice fl cd)) allMkts
    grads            = map (gradBP (cdsPrice fl cd)) allMkts


-- # TODO set the actual dates from the first set of markets
pltVectorMkt :: SimpleMarket -> Layout Rate Rate
pltVectorMkt mkt = do
    let ratesDerives   = set dates [0.1,0.5,1,1.5,2] $ view (irCurve)     $ mkt
        hazardsDerives = set dates [0.1,0.5,1,1.5,2] $ view (hazardRates) $ mkt
    execEC (chart ratesDerives hazardsDerives)

main :: IO ()
main =  do

    -- Bs call price test
    let bs = MP { _r = 0.03, _strike = 50, _sigma = 1, _currentTime = 0.0, _endTime = 1.0, _st = 40.0}
    --print $ gradBP callPrice bs

    -- Create out market from curves
    -- These are market forward rates
    let irCurve1      = Curve [0.1,0.5,1,1.5,2] [0.05, 0.05, 0.05, 0.05,  0.05]
        hazardRates1  = Curve [0.1,0.5,1,1.5,2] [0.01,0.015,0.02,0.025,0.03]
        mkt1          = SimpleMarket irCurve1 hazardRates1

--    plotCurve hazardRates1
--    plotCurve hazardRates1

    let irCurve2      = Curve [0.1,0.5,1,1.5,2] [0.08, 0.08,  0.08, 0.08,  0.08]
        hazardRates2  = Curve [0.1,0.5,1,1.5,2] [0.015,0.02,0.025,0.04,0.05]
        mkt2          = SimpleMarket irCurve2 hazardRates2

    -- number of intermediate market points
    let numPoints = 20

    -- create cashflows for fixed leg
    -- create credit data notional and recovery rate
    let fixedLegCashFlow = CashFlows [0.05,1,1.5,2] [2,3,4,5]
        creditData       = Credit 10 0.4
        result'          = evolveLinear fixedLegCashFlow creditData mkt1 mkt2 numPoints

    -- first is either time or dummy time
    let prices      = map thd3 result'
        pricesAccum = map (\a -> take a prices) [1..length result']
        result''    = zipWith (\(x,y,z) p2 -> (x,y,p2)   ) result' pricesAccum

    -- derivatives with respect to interest rate just one though
    -- set the dates to their original values
    --let rendererdImg  = map (toRenderable . (pltVectorMkt 1 1) . snd3) result'
    let fs = FillStyleSolid (opaque white)
    let rendererdImg  = map ( (fillBackground fs) . gridToRenderable . (grid (numPoints + 2))) result''

    defaultE <- defaultEnv bitmapAlignmentFns 2000 1000

    --renderToDynamicImage 100 100 rendererdImg
    let z :: [QDiagram CA.Cairo DP.V2 Double DP.Any] = map (\z-> fst $ runBackendR defaultE z) rendererdImg

    CmdInt.mainWith $ zip z [1..length z]

--    print $ evalBP2 integrateCurve hazardRates 3
--    print $ result
--    print $ evolveLinear fixedLegCashFlow creditData mkt
    print "finished"
