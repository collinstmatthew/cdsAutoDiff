{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Numeric.Backprop
import Control.Lens
import GHC.Generics

--https://www.johndcook.com/blog/2009/01/19/stand-alone-error-function-erf/
-- implemnentation of error function
erf x = sign * y where
    --constants
    a1 =  0.254829592
    a2 = -0.284496736
    a3 =  1.421413741
    a4 = -1.453152027
    a5 =  1.061405429
    p  =  0.3275911
    -- Save the sign of x
    sign = if x < 0 then -1 else 1
    abx  = abs x

    -- A & S 7.1.26
    t = 1.0/(1.0 + p*abx)
    y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp (-abx*abx)

normalCDF value = 0.5 * (1-erf (-value * sqrt 0.5 ))

-- Setup some dates for model
type Rate  = Double
type Price = Double
type Vol   = Double
type Time  = Double

-- should create a curve data type across maturities which will be the r
data ModelParams = MP { _r           :: Rate,
                        _strike      :: Price,
                        _sigma       :: Vol,
                        _currentTime :: Time,
                        _endTime     :: Time,
                        _st          :: Price} deriving (Show, Generic)

instance Backprop ModelParams
makeLenses ''ModelParams

-- a curve is just time rate points which we can then interpolate however we wish
data Curve = Curve { _dates :: [Time],
                     _rates :: [Rate]
                   } deriving (Show, Generic)

instance Backprop Curve
makeLenses ''Curve

-- Market object is made up of iterest and hazard rates
data Market = Market { _irCurve     :: Curve,
                       _hazardRates :: Curve } deriving (Show,Generic)

instance Backprop Market
makeLenses ''Market

data CashFlows = CashFlows { _cashDates :: [Time],
                             _quantity  :: [Price] } deriving (Show, Generic)

instance Backprop CashFlows
makeLenses ''CashFlows

dot :: Num a => [a] -> [a] -> a
dot x y = foldl (+) 0 $ zipWith (*) x y

m3 :: Num a => a -> a -> a -> a
m3 a b c = a * b * c

-- take a hazard curve and produces a survival rate at a given point in time
-- assums hazard rates are piecewise constant
survivalCurve :: Reifies s W => BVar s Curve -> BVar s Time -> BVar s Time
survivalCurve hazard t = exp (-(dot timesDiff hazardRates)) where
    hazardRates = tail $ take (numDate+1) (sequenceVar (hazard ^^. rates) )
    datesH      = sequenceVar $ hazard ^^. dates
    numDate     = length filteredT
    times       = filteredT ++ [datesH!!numDate]
    filteredT   = (filter ((<) t) datesH)
    times'      = [0] ++ (init times)
    timesDiff   = zipWith (-) times times'

-- Takes a curve a sums all the points on it dummy functional on curve
-- will become more complicated naturally
cashFlowValue :: Reifies s W => CashFlows -> BVar s Market -> BVar s Price
--fixedLegPrice cashflows ircurve = foldr (+) 0 (tenorRates) where
cashFlowValue cashflows mkt = discounted where
        tenorRates     = sequenceVar (mkt ^^. irCurve ^^. rates)
        -- should change this to cash flow payment times
        timesHazard    = sequenceVar (mkt ^^. hazardRates ^^. dates)
        survivalVals   = map (survivalCurve (mkt ^^. hazardRates )) timesHazard
        tenorCashFlows = sequenceVar $ (auto cashflows) ^^. quantity
        discounted     = foldr (+) 0 $  zipWith3 m3 tenorRates tenorCashFlows survivalVals


cdsPrice :: Reifies s W =>   CashFlows -> CashFlows -> BVar s Market -> BVar s Price
cdsPrice premiumleg creditLeg mkt = cashFlowValue premiumleg mkt + cashFlowValue creditLeg mkt

callPrice :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a -> a
callPrice r k sigma currentTime endTime st = (normalCDF d1) * st - (normalCDF d2) * pvk  where
    d1           = 1/(sigma * (sqrt tau)) * ( log (st/k)  + tau*(r+sigma*sigma/2) )
    d2           = d1 - sigma * (sqrt tau)
    tau          = endTime - currentTime
    pvk          = k * exp (-r * tau)

--first get term structure of interest rates
--https://backprop.jle.im/03-manipulating-bvars.html
--instead of hardcoding in what value to differentiate in pass the set of lenses to the function
delta :: ModelParams -> Price -> Price
delta bls z = gradBP (\x-> callPrice (bs ^^. r) (bs ^^. strike) (bs ^^. sigma) (bs ^^. currentTime) (bs ^^. endTime) x) z
    where
        bs = constVar $ bls

main :: IO ()
main = do

    let bs = MP { _r = 0.03, _strike = 50, _sigma = 1, _currentTime = 0.0, _endTime = 1.0, _st = 40.0}

    -- create an interest rate curve
    let irCurve     = Curve [0,0.5,1,1.5,2] [0.1,0.5,0.5,0.5,0.5]
    let hazardRates = Curve [0,0.5,1,1.5,2] [0.1,0.15,0.2,0.25,0.3]

    let mkt = Market irCurve hazardRates

    -- create cashflows
    let fixedLegCashFlow = CashFlows [0,0.5,1,1.5,2] [1,2,3,4,5]

    -- credit leg
    let notional = 10
    let recovery = 0.4
    let creditLegCashFlow = CashFlows [2] [-(1-recovery)*notional]

    print $ gradBP (cdsPrice fixedLegCashFlow creditLegCashFlow) mkt

    print (delta bs 80)

    print "finished"
