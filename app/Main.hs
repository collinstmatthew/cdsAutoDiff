{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- bs auto modules
import Market(SimpleMarket(..),Curve(..),irCurve,hazardRates,rates,dates)
import Trades.CallOption(callPrice,ModelParams(..))
import Trades.CDS(Credit(..))
import Trades.CashFlow(CashFlows(..))

import Types
import Evolution(evolveLinear,plotEvolution)


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

    -- create cashflows for fixed leg
    -- create credit data notional and recovery rate
    let fixedLegCashFlow = CashFlows [0.05,1,1.5,2] [2,3,4,5]
        creditData       = Credit 10 0.4
        numPoints = 2

    plotEvolution $ evolveLinear fixedLegCashFlow creditData mkt1 mkt2 numPoints

--    print $ evalBP2 integrateCurve hazardRates 3
--    print $ result
--    print $ evolveLinear fixedLegCashFlow creditData mkt
    print "finished"
