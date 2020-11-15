module Math(normalCDF,erf,difference,differenceR,dot,m3, genRange, maximum',minimum') where

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


minimum' :: Ord a => [a] -> a
minimum' = foldr1 (\x y ->if x <= y then x else y)

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y ->if x >= y then x else y)

-- take the difference of a list with a starting elemtn
difference ::Num a => Maybe a -> [a] -> [a]
difference (Just begin) l = zipWith (-) l (begin : init l)
difference Nothing l = zipWith (-) (tail l) (init l)

differenceR ::Num a => Maybe a -> [a] -> [a]
differenceR (Just begin) l = zipWith (flip (-)) l (begin : init l)
differenceR Nothing      l = zipWith (flip (-)) (tail l) (init l)

dot :: Num a => [a] -> [a] -> a
dot x y = sum $ zipWith (*) x y

m3 :: Num a => a -> a -> a -> a
m3 a b c = a * b * c

-- generage a range between two numbeers
genRange :: Double -> Double -> Double -> [Double]
genRange start end gap = takeWhile (< end ) [start + gap*n | n <- [1..]]
