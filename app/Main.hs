{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Numeric.Backprop
import Control.Lens
import GHC.Generics

import Debug.Trace

import Data.Sort(uniqueSort)
import Data.List(zipWith5)

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

-- get the appropriate value at the speicif time
getVal :: Reifies s W => BVar s Curve -> BVar s Time -> BVar s Rate
getVal curve time   | numG == 0 = last ratesG
                    | otherwise =  head $ drop numG ratesG where
    numG = length $ filter (time <) datesG
    datesG = sequenceVar (curve ^^. dates)
    ratesG = sequenceVar (curve ^^. rates)

-- Market object is made up of iterest and hazard rates
data Market = Market { _irCurve     :: Curve,
                       _hazardRates :: Curve } deriving (Show,Generic)

instance Backprop Market
makeLenses ''Market

data CashFlows = CashFlows { _cashDates :: [Time],
                             _quantity  :: [Price] } deriving (Show, Generic)


instance Backprop CashFlows
makeLenses ''CashFlows



data Credit = Credit { _notional :: Double,
                      _recoveryRate :: Double} deriving (Show, Generic)

-- credit data
instance Backprop Credit
makeLenses ''Credit

dot :: Num a => [a] -> [a] -> a
dot x y = sum $ zipWith (*) x y

m3 :: Num a => a -> a -> a -> a
m3 a b c = a * b * c

-- take the difference of a list with a starting elemtn
difference ::Num a => Maybe a -> [a] -> [a]
difference (Just begin) l = zipWith (-) l (begin : init l)
difference Nothing l = zipWith (-) (tail l) (init l)

differenceR ::Num a => Maybe a -> [a] -> [a]
differenceR (Just begin) l = zipWith (flip (-)) l (begin : init l)
differenceR Nothing      l = zipWith (flip (-)) (tail l) (init l)

-- take forward rates and gives a discount factor back
-- assums forward rates are piecewise constant
-- careful dot doesn't show error if sizes are difference
integrateCurve :: Reifies s W => BVar s Curve -> BVar s Time -> BVar s Rate
integrateCurve forwardRates t = exp (-(dot timesDiff forwardRates')) where
    forwardRates' = take (length times) (addDummy (sequenceVar (forwardRates ^^. rates) ))
    -- ensure if t is past the last node then add a dummy rate at the end
    addDummy x    = if t > last datesH then x ++ [last x] else x
    datesH        = sequenceVar $ forwardRates ^^. dates
    times         = filteredT ++ [t]
    filteredT     = filter (t >) datesH
    times'        = 0 : init times
    timesDiff     = difference (Just 0) times

-- get f_i h_i and b_i from the  curves
getFHB :: Reifies s W =>  BVar s Market -> [BVar s Time] -> ([BVar s Rate], [BVar s Rate], [BVar s Rate])
getFHB mkt joinDates = (fi,hi,bi) where
    hCurve    = mkt ^^. hazardRates
    iCurve    = mkt ^^. irCurve
    pi        = map (integrateCurve iCurve) joinDates
    qi        = map (integrateCurve hCurve) joinDates
    bi        = zipWith (*) pi qi
    fi        = differenceR Nothing $ map log pi
    hi        = differenceR Nothing $ map log qi

-- computes the discouunt factor of the protecitonLeg
--protectionLegDF :: Reifies s W => BVar s Market -> BVBar s Rate
protectionLegDF :: Reifies s W => BVar s Market -> BVar s Rate
protectionLegDF mkt = sum $ zipWith3 (\f h dB -> (h / (f+h)) * dB) fi hi diffBi  where
    diffBi    = differenceR (Just 1) bi
    joinDates = 0 : nodeDates mkt
    (fi,hi,bi) = getFHB mkt joinDates

nodeDates :: Reifies s W => BVar s Market -> [BVar s Time]
nodeDates mkt = uniqueSort (irDates ++ hazDates) where
    hCurve    = mkt ^^. hazardRates
    iCurve    = mkt ^^. irCurve
    hazDates  = sequenceVar $ hCurve ^^. dates
    irDates   = sequenceVar $ iCurve ^^. dates


accruedInterest :: Reifies s W => CashFlows -> BVar s Market -> BVar s Price
accruedInterest cf mkt = dot quantityCashFlows  (zipWith (*) eta accrP) where
    -- all the cash flow payment dates
    cfDates  = sequenceVar $ auto cf ^^. cashDates
    -- cash flow start and end dates
    cfStartEnd  = zip (0 : init cfDates) cfDates

    -- all the mkt not dates joint
    mktDates = nodeDates mkt

    -- all the market nodes in between coupon start and end dates inclusive
    accrNodes = map accrEx cfStartEnd
    accrEx (s,e) = [s] ++ [ i | i <- mktDates, s < i && i < e] ++ [e]

    -- sum over each period then will do product this with eta and multiply by coupon to get result
    accrP    = map (helperF mkt) accrNodes
    -- #TODO what is the difference between eta and Delta_i
    eta      = difference (Just 0) cfDates
    quantityCashFlows = sequenceVar $ auto cf ^^. quantity

helperF  :: Reifies s W => BVar s Market -> [BVar s Rate] -> BVar s Price
helperF mkt dates = sum res  where
    (fi,hi,bi) = getFHB mkt dates
    diffBi     = differenceR Nothing bi
    dti        = difference Nothing dates
    res        = zipWith5 (\f1 h1 b1 db1 dt1 -> (dt1 * h1)/(f1+h1) * (db1/(f1+h1) - b1)) fi hi bi diffBi dti

-- gets the yield instead of the discouunt factor
yieldCurve :: Reifies s W => BVar s Curve -> BVar s Time -> BVar s Rate
yieldCurve forwardRates t = (1/t) * log (integrateCurve forwardRates t)

-- Takes a curve a sums all the points on it dummy functional on curve
cashFlowValue :: Reifies s W => CashFlows -> BVar s Market -> BVar s Price
cashFlowValue cashflows mkt = discounted where
        timesCashFlows    = sequenceVar $ auto cashflows ^^. cashDates
        survivalProbs     = map (integrateCurve (mkt ^^. hazardRates )) timesCashFlows
        discountFact      = map (integrateCurve (mkt ^^. irCurve )) timesCashFlows

        quantityCashFlows = sequenceVar $ auto cashflows ^^. quantity
        discounted        = sum $  zipWith3 m3 discountFact quantityCashFlows survivalProbs

cdsPrice :: Reifies s W => CashFlows -> Credit -> BVar s Market -> BVar s Price
cdsPrice cashFlows creditData mkt = couponLeg - aI  - defaultLeg where
    couponLeg = cashFlowValue cashFlows mkt
    aI = accruedInterest cashFlows mkt
    defaultLeg = notional' * (1-rr) * protectionLegDF mkt where
        notional'  = cD ^^. notional
        rr   = cD ^^. recoveryRate
        cD   = auto creditData



callPrice  :: Reifies s W => BVar s ModelParams -> BVar s Price
callPrice bs = normalCDF d1 * str - normalCDF d2 * pvk  where
    d1           = 1/(sigma' * sqrt tau) * ( log (str/k)  + tau*(r'+sigma'*sigma'/2) )
    d2           = d1 - sigma' * sqrt tau
    tau          = endTime' - currentTime'
    pvk          = k * exp (-r' * tau)
    r'           = bs ^^. r
    k            = bs ^^. strike
    sigma'       = bs ^^. sigma
    currentTime' = bs ^^. currentTime
    endTime'     = bs ^^. endTime
    str          = bs ^^. st

main :: IO ()
main = do

    let bs = MP { _r = 0.03, _strike = 50, _sigma = 1, _currentTime = 0.0, _endTime = 1.0, _st = 40.0}

    let irCurve      = Curve [0.1,0.5,1,1.5,2] [0.5,0.5,0.5,0.5,0.5]
    let hazardRates  = Curve [0.1,0.5,1,1.5,2] [0.01,0.05,0.02,0.025,0.03]

    -- create an interest rate curve
    let mkt = Market irCurve hazardRates
             -- these are market forward rates

    -- create cashflows for fixed leg
    let fixedLegCashFlow = CashFlows [0.5,1,1.5,2] [2,3,4,5]
    -- create credit date
    let creditData       = Credit 10 0.4

    --print $ evalBP2 integrateCurve hazardRates 3

    -- credit leg

--    print $ evalBP (cdsPrice fixedLegCashFlow creditData) mkt
    print $ gradBP (cdsPrice fixedLegCashFlow creditData) mkt

    --print $ gradBP callPrice bs

    --print (delta bs 80)

    print "finished"
