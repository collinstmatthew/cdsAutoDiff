{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.Backprop
import Debug.Trace

-- bs auto modules
import Market(SimpleMarket(..),Curve(..),diffMarket,addMarket,divideMarket)
import Trades.CashFlow(CashFlows(..))
import Trades.CallOption(callPrice,ModelParams(..))
import Trades.CDS(Credit(..),cdsPrice)
import Types

import Data.List(iterate)

-- allow it so you can add two markets together with a type class
-- what to do if nodes are different? Take union of all nodes I guess
-- so first take difference between nodes, divide by n and then keep adding on

-- takes a starting market and an ending market and evoles the price linearly
-- using n number of points
-- at the moment just gives back the price at each market
--

evolveLinear :: CashFlows -> Credit -> SimpleMarket -> SimpleMarket -> Int -> [(SimpleMarket,SimpleMarket,Price)]
evolveLinear fl cd mktStart mktEnd n = zip3 allMkts grads prices where
    intermediateMkt  = divideMarket (fromIntegral n) $ diffMarket mktEnd mktStart
    allMkts          = take (n+2) $ iterate (addMarket intermediateMkt) mktStart
    prices           = map (evalBP (cdsPrice fl cd)) allMkts
    grads            = map (gradBP (cdsPrice fl cd)) allMkts


main :: IO ()
main = do

    -- Bs call price test
    let bs = MP { _r = 0.03, _strike = 50, _sigma = 1, _currentTime = 0.0, _endTime = 1.0, _st = 40.0}
    print $ gradBP callPrice bs

    -- Create out market from curves
    -- These are market forward rates
    let irCurve1      = Curve [0.1,0.5,1,1.5,2] [0.5,0.5,0.5,0.5,0.5]
        hazardRates1  = Curve [0.1,0.5,1,1.5,2] [0.01,0.05,0.02,0.025,0.03]
        mkt1 = SimpleMarket irCurve1 hazardRates1


    let irCurve2      = Curve [0.1,0.5,1,1.5,2] [0.8,0.8,0.8,0.8,0.8]
        hazardRates2  = Curve [0.1,0.5,1,1.5,2] [0.01,0.05,0.02,0.025,0.03]
        mkt2 = SimpleMarket irCurve2 hazardRates2

    -- create cashflows for fixed leg
    let fixedLegCashFlow = CashFlows [0.05,1,1.5,2] [2,3,4,5]
    -- create credit data notional and recovery rate
    let creditData       = Credit 10 0.4

    let result = gradBP (cdsPrice fixedLegCashFlow creditData) mkt1
    let result' = evolveLinear fixedLegCashFlow creditData mkt1 mkt2 3
    print result'

    --print $ evalBP2 integrateCurve hazardRates 3
--    print $ result
--    print $ evolveLinear fixedLegCashFlow creditData mkt

    print "finished"
