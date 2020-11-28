{-# LANGUAGE ScopedTypeVariables #-}

module Evolution(evolveLinear,plotEvolution) where

import Types
import Market(SimpleMarket(..),replaceDates,addMarket,divideMarket,diffMarket,plotPriceFakeT,plotPriceRealT,irCurve,hazardRates,plotCurve,getVal',dates,rates,Curve)
import Trades.CashFlow(CashFlows(..),cashDates)
import Trades.CDS(Credit(..),cdsPrice,CDS(..),premiumLeg)

import Numeric.Backprop

import Math(genRangeDay)

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

import Data.Time.Calendar(toModifiedJulianDay,Day(ModifiedJulianDay),diffDays)

type Evolution =(Either ([Time],Time) (Time,Time),[(SimpleMarket,SimpleMarket,Price)])

evolveLinear :: Time -> CDS -> SimpleMarket -> SimpleMarket -> Integer -> Maybe Time -> Evolution
evolveLinear pdate cds mktStart mktEnd n evEnd =(pDateEvolve', zip3 allMkts grads' prices) where
    -- pricing dates
    end = last $view cashDates $ view premiumLeg cds
    pDateEvolve  = case evEnd of
                      Just e  -> genRangeDay pdate e n
                      Nothing -> take (fromInteger (n+2)) $ repeat pdate
    pDateEvolve' = case evEnd of
                     Just e -> Left (pDateEvolve,end)
                     Nothing -> Right $ (pdate,end)


    intermediateMkt  = divideMarket (fromIntegral (n+1)) $ diffMarket mktEnd mktStart
    allMkts          = take (fromInteger (n+2)) $ iterate (addMarket intermediateMkt) mktStart

    prices           = zipWith (\x y -> evalBP (cdsPrice y cds) x) allMkts pDateEvolve
    grads            = zipWith (\x y -> gradBP (cdsPrice y cds) x) allMkts pDateEvolve

    -- replace the dates in the gradient market with the original dates
    grads'           = zipWith replaceDates allMkts grads

limitsIrHaz :: SimpleMarket -> SimpleMarket -> ((Rate,Rate),(Rate,Rate))
limitsIrHaz mktS mktE = ((minimum ir, maximum ir),(minimum hz, maximum hz)) where
    ir = view (irCurve . rates) mktS ++ view (irCurve . rates) mktE
    hz = view (hazardRates . rates) mktS ++ view (hazardRates . rates) mktE

limitsTimeIrHaz :: SimpleMarket -> SimpleMarket -> (Time,Time)
limitsTimeIrHaz mktS mktE = (minimum (ir ++ hz), maximum (ir ++ hz))  where
    ir = view (irCurve . dates) mktS ++ view (irCurve . dates) mktE
    hz = view (hazardRates . dates) mktS ++ view (hazardRates . dates) mktE

-- plots a real evolugion of a market
plotEvolution (Left (times,end),evolution) = do
    -- limits of the rates in the evolution for plotting
    let limits = limitsIrHaz (fst3 (head evolution)) (fst3 (last evolution))
    --let tenorLimits = (head times, last times)
    let tenorLimits = (head times, end)

    -- this should be the max element of times not he number of points now
    let numPoints = last times
        -- first is either time or dummy time
    let prices      = zip times (map thd3 evolution)
        priceLims   = (minimum (map snd prices), maximum (map snd prices))
        pricesAccum = map (\a -> take a prices) [1..length evolution]
        result''    = zipWith (\(x,y,z) p2 -> (x,y,p2)   ) evolution pricesAccum

    -- maximum length of the vector on the first derivative market so I can scale whole time evolution
    let (maxX,maxY)= maxVecLength tenorLimits (snd3 (head evolution)) ef'

    let renderedRes = map (\x -> (priceRenderableRealT priceLims numPoints x,rateRenderable tenorLimits limits x,vectorRenderable (maxX,maxY) tenorLimits x)) result''

    defaultEVec   <- defaultEnv bitmapAlignmentFns 2000 2400
    defaultERate  <- defaultEnv bitmapAlignmentFns 2000 1600
    defaultEPrice <- defaultEnv bitmapAlignmentFns 2000 800

    -- put all prices into the same list need this type annotion to specify the back end
    let zAll :: [(DT.QDiagram CA.Cairo DP.V2 Double DP.Any,DT.QDiagram CA.Cairo DP.V2 Double DP.Any,DT.QDiagram CA.Cairo DP.V2 Double DP.Any)] = map (\(x,y,z) -> (fst $ runBackendR defaultEPrice x,fst $ runBackendR defaultERate y,fst $ runBackendR defaultEVec z)) renderedRes

    let zTotalAll = map (\(x,y,z) -> z DP.||| (x DP.=== y)) zAll

    --CmdInt.mainWith $ zip zTotal [1..length zTotal]
    CmdInt.mainWith $ zip zTotalAll [1..length zTotalAll]

-- plots a fake evolugion of a market
plotEvolution (Right (pdate,end),evolution) = do
    let times::[Double] = take (length evolution)  [0..]
    -- limits of the rates in the evolution for plotting
    let limits = limitsIrHaz (fst3 (head evolution)) (fst3 (last evolution))
    --let tenorLimits =(pdate, snd $ limitsTimeIrHaz (fst3 (head evolution)) (fst3 (last evolution)))
    let tenorLimits =(pdate, end)

    let maxTime = length evolution -1
        -- first is either time or dummy time
    let prices      = zip times (map thd3 evolution)
        priceLims   = (minimum (map snd prices), maximum (map snd prices))
        pricesAccum = map (\a -> take a prices) [1..length evolution]
        result''    = zipWith (\(x,y,z) p2 -> (x,y,p2)   ) evolution pricesAccum

    -- maximum length of the vector on the first derivative market so I can scale whole time evolution
    let (maxX,maxY) = maxVecLength tenorLimits (snd3 (head evolution)) ef'

    let renderedRes = map (\x -> (priceRenderableFakeT priceLims maxTime x,rateRenderable tenorLimits limits x,vectorRenderable (maxX,maxY) tenorLimits x)) result''

    defaultEVec   <- defaultEnv bitmapAlignmentFns 2000 2400
    defaultERate  <- defaultEnv bitmapAlignmentFns 2000 1600
    defaultEPrice <- defaultEnv bitmapAlignmentFns 2000 800

    -- put all prices into the same list need this type annotion to specify the back end
    let zAll :: [(DT.QDiagram CA.Cairo DP.V2 Double DP.Any,DT.QDiagram CA.Cairo DP.V2 Double DP.Any,DT.QDiagram CA.Cairo DP.V2 Double DP.Any)] = map (\(x,y,z) -> (fst $ runBackendR defaultEPrice x,fst $ runBackendR defaultERate y,fst $ runBackendR defaultEVec z)) renderedRes

    let zTotalAll = map (\(x,y,z) -> z DP.||| (x DP.=== y)) zAll

    --CmdInt.mainWith $ zip zTotal [1..length zTotal]
    CmdInt.mainWith $ zip zTotalAll [1..length zTotalAll]


-- functions for plotting evolutiton
-- # TODO what if the tenors are different for irCurve and hazardRates

-- scaled the vectors so that each component is an integer between -100 and 100
-- take a grid and a function and returns a function from grid -> Grid
scaleVecs :: Double -> Double -> Double -> ((Time,Time) -> (Double,Double)) -> ((Time,Time) -> (Time,Time))
scaleVecs tick maxX maxY f1 =  transform . f1         where
    -- make the original size half of the maximum it could overlap to
    factor = 0.5
    scaleX = factor * tick / maxX
    scaleY = factor * tick / maxY
    transform (x,y) = (timeValueFromDouble (scaleX *x), timeValueFromDouble (scaleY *y))


square :: Time -> Time -> SimpleMarket -> Integer -> [(Time, Time)]
square start end mkt space =  [(x,y) | x <- axis, y <- axis]  where
    -- generate 30 points on the grid
    axis = genRangeDay start end space

ef' :: Curve -> Curve -> (Time,Time) -> (Double,Double)
ef' derivCurve hazardCurve (x,y) = ( (getVal' derivCurve y),(getVal' hazardCurve x))

vectorField start end mkt title f = fmap plotVectorField $ liftEC $ do
    c <- takeColor
    plot_vectors_mapf  .= f
    plot_vectors_scale .= 0
    plot_vectors_grid  .= square start end mkt 20
    plot_vectors_title .= title
    plot_vectors_style . vector_line_style . line_color .= c
    plot_vectors_style . vector_head_style . point_color .= c

-- filter the axis so it's only for dates between start and end date don't know why more
myaxisvals start end x =  autoTimeValueAxis $ filter (\y -> y >= start && y <= end) x


-- Get the maximum vector legnth from the evolution so we can scale for between 0 and 20
maxVecLength :: (Time,Time) -> SimpleMarket -> (Curve -> Curve -> (Time,Time) -> (Double,Double)) -> (Double,Double)
maxVecLength (start,end) market f1 = (maximum allX, maximum allY) where
    ir   = view irCurve market
    hz   = view hazardRates market
    grid = square start end market 20

    allVecs = map (f1 ir hz) grid
    -- gets the absolutes of everything
    (allX,allY) = unzip $ map (\(x,y) -> (abs x, abs y)) allVecs



plotVec maxX maxY start end mkt = do

        setColors [opaque blue]
--        layout_title .= "Derivatives of cds evolution"
        layout_y_axis . laxis_generate  .= myaxisvals start end
        layout_y_axis . laxis_title     .= "IR Sensitivites"
        layout_y_axis . laxis_style . axis_label_style . font_size  .= 36
        layout_y_axis . laxis_title_style . font_size .= 42


        layout_x_axis . laxis_generate  .= myaxisvals start end
        layout_x_axis . laxis_title     .= "Hazard Sensitivites"
        layout_x_axis . laxis_style . axis_label_style . font_size  .= 36
        layout_x_axis . laxis_title_style . font_size .= 42

        let deri = view irCurve mkt
            heri = view hazardRates mkt

        let gap = 20
        -- ticks size on the grid for scaling the vectors
        let ticks = (fromIntegral (diffDays end start)) / (fromIntegral (gap + 1))

        let myFun = scaleVecs ticks maxX maxY (ef' deri heri)

        plot $ vectorField start end mkt "" myFun


rateRenderable :: (Time,Time) -> ((Rate,Rate),(Rate,Rate)) -> (SimpleMarket,b,c) -> Renderable (LayoutPick Time Rate Rate)
rateRenderable tenorLimits limits (mktOrig,mktDeriv,price) = ((fillBackground ( FillStyleSolid (opaque white) )) .  gridToRenderable) (aboveN [irP,hzP])
  where
    (irLimits,hzLimits)  = limits
    irP        = tspan (layoutToRenderable (plotCurve "Foward Interest Rate" tenorLimits irLimits mktirCurve)) (1,1)
    hzP        = tspan (layoutToRenderable (plotCurve "Forward Hazard Rate" tenorLimits hzLimits mkthzCurve)) (1,1)
    mktirCurve = view irCurve mktOrig
    mkthzCurve = view hazardRates mktOrig

priceRenderableFakeT :: (Price,Price) -> Int -> (SimpleMarket,b,[(Double,Price)]) -> Renderable (LayoutPick Double Price Price)
priceRenderableFakeT pricelims maxTime (_,_,price) = ((fillBackground ( FillStyleSolid (opaque white) )) .  gridToRenderable) (priceP)
  where
    priceP     = tspan (layoutToRenderable (plotPriceFakeT pricelims maxTime price)) (1,1)


priceRenderableRealT :: (Price,Price) -> Time -> (SimpleMarket,b,[(Time,Price)]) -> Renderable (LayoutPick Time Price Price)
priceRenderableRealT pricelims maxTime (_,_,price) = (fillBackground ( FillStyleSolid (opaque white) ) .  gridToRenderable) (priceP)
  where
    priceP     = tspan (layoutToRenderable (plotPriceRealT pricelims maxTime price)) (1,1)

vectorRenderable ::(Double,Double) -> (Time,Time) -> (SimpleMarket,SimpleMarket,a) -> Renderable (LayoutPick Time Time Time)
vectorRenderable (maxX,maxY) (start,end) (_,mktDeriv,_) = (fillBackground ( FillStyleSolid (opaque white)) .  gridToRenderable) vectorP
  where
    vectorP    = tspan (layoutToRenderable (execEC (plotVec maxX maxY start end mktDeriv))) (1,1)
