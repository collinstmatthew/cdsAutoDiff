{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Trades.CallOption(ModelParams(..),callPrice) where

import Numeric.Backprop
import Control.Lens
import GHC.Generics
import Types
import Math(normalCDF)

-- should create a curve data type across maturities which will be the r
data ModelParams = MP { _r           :: Rate,
                        _strike      :: Price,
                        _sigma       :: Vol,
                        _currentTime :: Time,
                        _endTime     :: Time,
                        _st          :: Price} deriving (Show, Generic)

instance Backprop ModelParams
makeLenses ''ModelParams

callPrice  :: Reifies s W => BVar s ModelParams -> BVar s Price
callPrice bs = normalCDF d1 * str - normalCDF d2 * pvk  where
    d1           = 1/(sigma' * sqrt tau) * ( log (str/k)  + tau*(r'+sigma'*sigma'/2) )
    d2           = d1 - sigma' * sqrt tau
    tau          = dateFrac currentTime' endTime' ACT365F
    pvk          = k * exp (-r' * tau)
    r'           = bs ^^. r
    k            = bs ^^. strike
    sigma'       = bs ^^. sigma
    currentTime' = bs ^^. currentTime
    endTime'     = bs ^^. endTime
    str          = bs ^^. st

