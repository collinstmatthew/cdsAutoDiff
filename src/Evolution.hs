module Evolution(evolveLinear) where

import Types
import Market(SimpleMarket(..),replaceDates,addMarket,divideMarket,diffMarket)
import Trades.CashFlow(CashFlows(..))
import Trades.CDS(Credit(..),cdsPrice)

import Numeric.Backprop

evolveLinear :: CashFlows -> Credit -> SimpleMarket -> SimpleMarket -> Int -> [(SimpleMarket,SimpleMarket,Price)]
evolveLinear fl cd mktStart mktEnd n = zip3 allMkts grads' prices where
    intermediateMkt  = divideMarket (fromIntegral n) $ diffMarket mktEnd mktStart
    allMkts          = take (n+2) $ iterate (addMarket intermediateMkt) mktStart
    prices           = map (evalBP (cdsPrice fl cd)) allMkts
    grads            = map (gradBP (cdsPrice fl cd)) allMkts
    -- replace the dates in the gradient market with the original dates
    grads'           = zipWith replaceDates allMkts grads

