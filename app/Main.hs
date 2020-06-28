{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Numeric.Backprop
import Numeric.AD
import Numeric.AD.Internal.Forward
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

    print (delta bs 80)

    print "finished"
