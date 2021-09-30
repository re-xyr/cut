{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
module Autodiff where

import           Data.Number.Erf

class Autodiff d where
  type Scalar d
  constant :: Scalar d -> d
  value :: d -> Scalar d
  elementary :: (Scalar d -> Scalar d)
    -> (Scalar d -> Scalar d)
    -> d -> d
  arithmetic :: (Scalar d -> Scalar d -> Scalar d)
    -> (Scalar d -> Scalar d -> (Scalar d, Scalar d))
    -> d -> d -> d

class Autodiff d => Constable d where
  isConstant :: d -> Maybe (Scalar d)

pattern Constant :: Constable d => Scalar d -> d
pattern Constant x <- (isConstant -> Just x)

newtype ByAutodiff d = ByAutodiff d
  deriving (Autodiff, Constable)

squared :: Num a => a -> a
squared x = x * x

instance (Autodiff d, Eq (Scalar d)) => Eq (ByAutodiff d) where
  u == v = value u == value v

instance (Autodiff d, Ord (Scalar d)) => Ord (ByAutodiff d) where
  compare u v = compare (value u) (value v)

instance (Autodiff d, Num (Scalar d), Enum (Scalar d)) => Enum (ByAutodiff d) where
  toEnum = constant . toEnum
  fromEnum = fromEnum . value

instance (Autodiff d, Bounded (Scalar d)) => Bounded (ByAutodiff d) where
  minBound = constant minBound
  maxBound = constant maxBound

instance (Autodiff d, Num (Scalar d)) => Num (ByAutodiff d) where
  -- d(u+v) = du + dv
  (+) = arithmetic (+) \_ _ -> (1, 1)
  -- d(u-v) = du - dv
  (-) = arithmetic (-) \_ _ -> (1, -1)
  -- d(uv) = v(du) + u(dv)
  (*) = arithmetic (*) \u v -> (v, u)
  -- d(-x) = -1
  negate = elementary negate (const (-1))
  -- d(|x|) = sig x (x != 0)
  abs = elementary abs signum
  -- d(sig x) = 0 (x != 0)
  signum = elementary signum (const 0)
  fromInteger = constant . fromInteger

instance (Autodiff d, Real (Scalar d)) => Real (ByAutodiff d) where
  toRational = toRational . value

instance (Autodiff d, Fractional (Scalar d)) => Fractional (ByAutodiff d) where
  -- d(1/x) = -1 / x^2
  recip = elementary recip (negate . recip . squared)
  -- d(u/v) = (v(du) - u(dv)) / v^2
  (/) = arithmetic (/) \u v -> (recip v, -u / squared v)
  fromRational = constant . fromRational

instance (Constable d, Floating (Scalar d)) => Floating (ByAutodiff d) where
  pi = constant pi
  -- d(exp x) = exp x
  exp = elementary exp exp
  -- d(x^n) = nx^(n-1)
  x ** Constant y = elementary (** y) (\x' -> x' ** (y - 1) * y) x
  -- d(u^v) = (u ^ (v-1)) * (v(du) + u(log u)(dv))
  x ** y = arithmetic (**) (\u v -> (u ** (v - 1) * v, u ** v * log u)) x y
  -- d(ln x) = 1/x
  log = elementary log recip
  -- d(log_u v) = ((ln v)(du)/u - (ln u)(dv)/v) / (ln u)^2
  logBase = arithmetic logBase \u v -> (-log v / u / squared (log u), recip (v * log u))
  -- d(sqrt x) = 1 / 2(sqrt x)
  sqrt = elementary sqrt \x -> recip $ 2 * sqrt x
  -- d(sin x) = cos x
  sin = elementary sin cos
  -- d(cos x) = -(sin x)
  cos = elementary cos (negate . sin)
  -- d(tan x) = (sec x)^2
  tan = elementary tan (recip . squared . cos)
  -- d(arcsin x) = 1 / sqrt(1 - x^2)
  asin = elementary asin \x -> recip $ sqrt $ 1 - squared x
  -- d(arccos x) = -1 / sqrt(1 - x^2)
  acos = elementary acos \x -> negate $ recip $ sqrt $ 1 - squared x
  -- d(arctan x) = 1 / (1 + x^2)
  atan = elementary atan \x -> recip $ 1 + squared x
  -- d(sinh x) = cosh x
  sinh = elementary sinh cosh
  -- d(cosh x) = sinh x
  cosh = elementary cosh sinh
  -- d(tanh x) = (sech x)^2
  tanh = elementary tanh (recip . squared . cosh)
  -- d(arcsinh x) = 1 / sqrt(x^2 + 1)
  asinh = elementary asinh \x -> recip $ sqrt $ squared x + 1
  -- d(arccosh x) = 1 / sqrt(x^2 - 1)
  acosh = elementary acosh \x -> recip $ sqrt $ squared x - 1
  -- d(arctanh x) = 1 / (1 - x^2)
  atanh = elementary atanh \x -> recip $ 1 - squared x

instance (Constable d, Erf (Scalar d)) => Erf (ByAutodiff d) where
  erf = elementary erf \x -> 2 / sqrt pi * exp (-squared x)
  erfc = elementary erfc \x -> (-2) / sqrt pi * exp (-squared x)
  normcdf = elementary normcdf \x -> recip (sqrt (2 * pi)) * exp (-x * x / 2)

instance (Constable d, InvErf (Scalar d)) => InvErf (ByAutodiff d) where
  inverf = elementary inverf \x -> sqrt pi / 2 * exp (squared x)
  inverfc = elementary inverfc \x -> (-sqrt pi / 2) * exp (squared x)
  invnormcdf = elementary invnormcdf \x -> sqrt (2 * pi) * exp (squared x / 2)
