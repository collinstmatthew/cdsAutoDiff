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

data CashFlows = CashFlows { _cashDates :: [Time],
                             _quantity  :: [Price] } deriving (Show, Generic)

instance Backprop CashFlows
makeLenses ''CashFlows

-- Takes a curve a sums all the points on it dummy functional on curve
cashFlowValue :: Reifies s W => CashFlows -> BVar s SimpleMarket -> BVar s Price
cashFlowValue cashflows mkt = discounted where
        timesCashFlows    = sequenceVar $ auto cashflows ^^. cashDates
        survivalProbs     = map (integrateCurve (mkt ^^. hazardRates )) timesCashFlows
        discountFact      = map (integrateCurve (mkt ^^. irCurve )) timesCashFlows

        quantityCashFlows = sequenceVar $ auto cashflows ^^. quantity
        discounted        = sum $  zipWith3 m3 discountFact quantityCashFlows survivalProbs
