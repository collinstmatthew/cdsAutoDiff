{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- bs auto modules
import Market(SimpleMarket(..),Curve(..),plotPrice,plotCurve,irCurve,hazardRates,rates,getVal',dates,replaceDates)
import Trades.CallOption(callPrice,ModelParams(..))
import Trades.CashFlow(CashFlows(..))
import Data.List(iterate)

import Types
import Evolution(evolveLinear)
import Data.Tuple.Extra

import Trades.CDS(Credit(..))

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

-- square for the graph
square :: [(Rate, Rate)]
square = [(x,y) | x <- axis, y <- axis] where
    axis = [ 0.1*x' | x' <- [1..20]]

ef derivCurve hazardCurve (x,y) = (getVal' derivCurve y,getVal' hazardCurve x)

vectorField title f = fmap plotVectorField $ liftEC $ do
    c <- takeColor
    plot_vectors_mapf  .= f
    plot_vectors_scale .= 1
    plot_vectors_grid  .= square
    plot_vectors_title .= title
    plot_vectors_style . vector_line_style . line_color .= c
    plot_vectors_style . vector_head_style . point_color .= c

chart mkt = do
        let deri = view irCurve mkt
        let heri = view hazardRates mkt
        setColors [opaque black, opaque blue]
--        layout_title .= "Derivatives of cds evolution"
        layout_y_axis . laxis_generate  .= scaledAxis def (0,2)
        layout_y_axis . laxis_title     .= "IR Sensitivites"
        layout_x_axis . laxis_generate  .= scaledAxis def (0,2)
        layout_x_axis . laxis_title     .= "Hazard Sensitivites"
        plot $ vectorField "" (ef deri heri)

-- Construct a grid of charts, with a single title accross the top
grid :: Int -> (SimpleMarket,SimpleMarket,[Price]) -> Grid ( Renderable (LayoutPick Rate Rate Rate))
grid maxTime (mktOrig,mktDeriv,price) = title `wideAbove` (besideN [ vectorP, aboveN [priceP,irP,hzP]])
  where
    irP        = tspan (layoutToRenderable (plotCurve "Interest rates" mktirCurve)) (1,1)
    hzP        = tspan (layoutToRenderable (plotCurve "Hazard rate" mkthzCurve)) (1,1)
    vectorP    = tspan (layoutToRenderable (execEC (chart mktDeriv))) ((1,3))
    priceP     = tspan (layoutToRenderable (plotPrice maxTime price)) (1,1)
    mktirCurve = view irCurve mktOrig
    mkthzCurve = view hazardRates mktOrig
    title      = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre "CDS sensitivies"
    ls         = def { _font_size   = 15 , _font_weight = FontWeightBold }

main :: IO ()
main =  do

    -- Bs call price test
    let bs = MP { _r = 0.03, _strike = 50, _sigma = 1, _currentTime = 0.0, _endTime = 1.0, _st = 40.0}
    --print $ gradBP callPrice bs

    -- Create out market from curves
    -- These are market forward rates
    let irCurve1      = Curve [0.1,0.5,1,1.5,2] [0.05,0.05,0.05,0.05,0.05]
        hazardRates1  = Curve [0.1,0.5,1,1.5,2] [0.01,0.015,0.02,0.025,0.03]
        mkt1          = SimpleMarket irCurve1 hazardRates1

    let irCurve2      = Curve [0.1,0.5,1,1.5,2] [0.08,0.08,0.08,0.08,0.08]
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

    let fs           = FillStyleSolid (opaque white)
        rendererdImg  = map ( (fillBackground fs) . gridToRenderable . (grid (numPoints + 2))) result''

    defaultE <- defaultEnv bitmapAlignmentFns 2000 1000

    -- Type annotation is needed to set backend
    let z :: [QDiagram CA.Cairo DP.V2 Double DP.Any] = map (\z-> fst $ runBackendR defaultE z) rendererdImg

    CmdInt.mainWith $ zip z [1..length z]

--    print $ evalBP2 integrateCurve hazardRates 3
--    print $ result
--    print $ evolveLinear fixedLegCashFlow creditData mkt
    print "finished"
