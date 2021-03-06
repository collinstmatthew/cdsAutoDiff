{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}


module Trades.CashFlow(CashFlows(..),cashFlowValue,cashDates,quantity) where

import Numeric.Backprop
import Control.Lens
import GHC.Generics
import Types

import Market(SimpleMarket,integrateCurve,hazardRates,irCurve)
import Math(m3)

import Debug.Trace

data CashFlows = CashFlows { _cashDates :: [Time],
                             _quantity  :: [Price] } deriving (Show, Generic)

instance Backprop CashFlows
makeLenses ''CashFlows

-- takes two lists cuts off elements from b to make it the same length as a
cutStart :: [a] -> [b] -> [b]
cutStart l1 l2 = drop nD l2 where
    nA = length l1
    nD = length l2 - nA

-- Takes a curve a sums all the points on it dummy functional on curve
cashFlowValue :: Reifies s W => Time -> CashFlows -> BVar s SimpleMarket -> BVar s Price
cashFlowValue pDate cashflows mkt = discounted where
        (cfDates,cfQuant) = unzip $ filter(\x-> fst x >= pDate)$ zip (view cashDates cashflows) (view quantity cashflows)

        -- get cash flow times and discouunt and survival factors
        timesCashFlows    = sequenceVar $ auto cfDates
        survivalProbs     = map (integrateCurve (mkt ^^. hazardRates) (auto pDate)) timesCashFlows
        discountFact      = map (integrateCurve (mkt ^^. irCurve) (auto pDate)) timesCashFlows

        -- return final value of discounted cash flows
        quantityCashFlows = sequenceVar $ auto cfQuant
        discounted        = sum $ zipWith3 m3 discountFact quantityCashFlows survivalProbs


