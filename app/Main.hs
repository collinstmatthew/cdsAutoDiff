module Main where

import Numeric.Backprop
import Debug.Trace

-- bs auto modules
import Market(SimpleMarket(..),Curve(..))
import Trades.CashFlow(CashFlows(..))
import Trades.CallOption(callPrice,ModelParams(..))
import Trades.CDS(Credit(..),cdsPrice)

main :: IO ()
main = do

    -- Bs call price test
    let bs = MP { _r = 0.03, _strike = 50, _sigma = 1, _currentTime = 0.0, _endTime = 1.0, _st = 40.0}
    print $ gradBP callPrice bs

    -- Create out market from curves
    -- These are market forward rates
    let irCurve      = Curve [0.1,0.5,1,1.5,2] [0.5,0.5,0.5,0.5,0.5]
    let hazardRates  = Curve [0.1,0.5,1,1.5,2] [0.01,0.05,0.02,0.025,0.03]

    -- create an interest rate curve
    let mkt = SimpleMarket irCurve hazardRates

    -- create cashflows for fixed leg
    let fixedLegCashFlow = CashFlows [0.05,1,1.5,2] [2,3,4,5]
    -- create credit data notional and recovery rate
    let creditData       = Credit 10 0.4

    --print $ evalBP2 integrateCurve hazardRates 3
    print $ gradBP (cdsPrice fixedLegCashFlow creditData) mkt

    print "finished"
