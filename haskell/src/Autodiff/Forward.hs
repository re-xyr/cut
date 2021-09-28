{-# LANGUAGE BlockArguments #-}
module Autodiff.Forward where

import qualified Debug.SimpleReflect as Reflect

data Diff a = Diff
  { getGradient :: a
  , getValue    :: !a
  } deriving (Show)

-- dC/dx = 0
constant :: Num a => a -> Diff a
constant = Diff 0

-- dx/dx = 1
independent :: Num a => a -> Diff a
independent = Diff 1

-- du/dx = du/dv * dv/dx
elementary :: Num a => (a -> a) -> (a -> a) -> Diff a -> Diff a
elementary f f' (Diff dy y) = Diff (f' y * dy) (f y)

arithmetic :: Num a => (a -> a -> a) -> (a -> a -> (a, a)) -> Diff a -> Diff a -> Diff a
arithmetic f f' (Diff du u) (Diff dv v) =
  let (dx, dy) = f' u v in Diff (dx * du + dy * dv) (f u v)

squared :: Num a => a -> a
squared x = x * x

instance Eq a => Eq (Diff a) where
  u == v = getValue u == getValue v

instance Ord a => Ord (Diff a) where
  compare u v = compare (getValue u) (getValue v)

instance (Num a, Enum a) => Enum (Diff a) where
  toEnum = constant . toEnum
  fromEnum d = fromEnum (getGradient d)

instance (Num a, Bounded a) => Bounded (Diff a) where
  minBound = constant minBound
  maxBound = constant maxBound

instance Num a => Num (Diff a) where
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

instance Real a => Real (Diff a) where
  toRational = toRational . getValue

instance Fractional a => Fractional (Diff a) where
  -- d(1/x) = -1 / x^2
  recip = elementary recip (negate . recip . squared)
  -- d(u/v) = (v(du) - u(dv)) / v^2
  (/) = arithmetic (/) \u v -> (recip v, -u / squared v)
  fromRational = constant . fromRational

instance Floating a => Floating (Diff a) where
  pi = constant pi
  -- d(exp x) = exp x
  exp = elementary exp exp
  -- d(u^v) = (u ^ (v-1)) * (v(du) + u(log u)(dv))
  (**) = arithmetic (**) \u v -> (u ** (v - 1) * v, u ** (v - 1) * u * log u)
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

diff' :: Num a => (Diff a -> Diff a) -> a -> Diff a
diff' f = f . independent

-- >>> diff' sin 0
-- >>> diff' exp 0
-- >>> diff' (exp . log) 2
-- Diff {getGradient = 1.0, getValue = 0.0}
-- Diff {getGradient = 1.0, getValue = 1.0}
-- Diff {getGradient = 1.0, getValue = 2.0}

diff :: Num a => (Diff a -> Diff a) -> a -> a
diff f = getGradient . diff' f

-- >>> let t = 2.0 in diff (\x -> constant t * sin x) 0
-- >>> diff (diff (diff sin)) 1
-- >>> diff (\x -> x ** x) 5
-- >>> diff (\x -> exp x / x ^ 2) 5
-- >>> diff (logBase 10) 114514
-- 2.0
-- -0.5403023058681398
-- 8154.493476356563
-- 3.5619158184618382
-- 3.7925011955154114e-6

diffSym :: (Diff Reflect.Expr -> Diff Reflect.Expr) -> Reflect.Expr
diffSym f = diff f Reflect.x

-- >>> let t = 2.0 in diffSym (\x -> constant t * sin x)
-- >>> diffSym (diff (diff sin))
-- >>> diffSym (\x -> x ** x)
-- >>> diffSym (\x -> exp x / x ^ 2)
-- >>> diffSym (logBase 10)
-- sin x * 0 + 2.0 * (cos x * 1)
-- 1 * (negate (sin x) * 1 * 0 + 1 * (1 * (negate 1 * (cos x * 1)) + negate (sin x) * 0)) + 1 * (0 * (negate (sin x) * 1) + cos x * 0)
-- x**(x - 1) * x * 1 + x**(x - 1) * x * log x * 1
-- recip (x * x) * (exp x * 1) + negate (exp x / (x * x * (x * x))) * (x * 1 + x * 1)
-- negate (log x / 10 / (log 10 * log 10)) * 0 + recip (x * log 10) * 1
