{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import qualified Autodiff.Forward as Forward
import           Criterion.Main
import           Data.Number.Erf

blackScholes :: (Erf a) => a -> a -> a -> a -> a -> (a, a)
blackScholes r s v t k = (put, call)
  where
    put = k * exp (negate r * t) - s + call
    call = normcdf (negate d2) * k * exp (negate r * t) - normcdf (negate d1) * s
    d1 = (log (s / k) + (r + v * v / 2) * t) / (v * sqrt t)
    d2 = d1 - v * t

bs :: Erf a => [a] -> (a, a)
bs [r', s', v', t', k'] = blackScholes r' s' v' t' k'

fromPair :: (t, t) -> [t]
fromPair (a, b) = [a, b]

runF :: Num a => (a -> a -> a -> a -> a -> b) -> Int -> [b]
runF f n =
    [ f r s v t k
    | r <- xs, s <- xs, v <- xs, t <- xs, k <- xs]
  where
    xs = map fromIntegral [1..n]

runFloat :: (Float -> Float -> Float -> Float -> Float -> b) -> Int -> [b]
runFloat = runF

runDouble :: (Double -> Double -> Double -> Double -> Double -> b) -> Int -> [b]
runDouble = runF

main :: IO ()
main = defaultMain
  [ bgroup "Forward"
    [ bench "greeks Double" $ nf (runDouble $ \r s v t k -> Forward.jacobian (fromPair . bs) [r, s, v, t, k]) 2
    , bench "higherGreeks Double" $ nf (runDouble $ \r s v t k -> (Forward.hessian (fst . bs) [r, s, v, t, k], Forward.hessian (snd . bs) [r, s, v, t, k])) 2
    , bench "highererGreeks Double" $ nf (runDouble $ \r s v t k -> Forward.hessianVec (fromPair . bs) [r, s, v, t, k]) 2
    , bench "greeks Float" $ nf (runFloat $ \r s v t k -> Forward.jacobian (fromPair . bs) [r, s, v, t, k]) 2
    , bench "higherGreeks Float" $ nf (runFloat $ \r s v t k -> (Forward.hessian (fst . bs) [r, s, v, t, k], Forward.hessian (snd . bs) [r, s, v, t, k])) 2
    , bench "highererGreeks Float" $ nf (runFloat $ \r s v t k -> Forward.hessianVec (fromPair . bs) [r, s, v, t, k]) 2
    ]
  ]
