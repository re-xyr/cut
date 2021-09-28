{-# LANGUAGE BlockArguments #-}
module Autodiff.Forward where

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
elementary f f' (Diff dy y) = Diff (dy * f' y) (f y)

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
  Diff du u + Diff dv v = Diff (du + dv) (u + v)
  -- d(uv) = u(dv) + v(du)
  Diff du u * Diff dv v = Diff (u * dv + v * du) (u * v)
  -- d(-x) = -1
  negate = elementary negate (const (-1))
  -- d(|x|) = sig x (except for x=0)
  abs = elementary abs signum
  -- d(sig x) = 0 (except for x=0)
  signum = elementary signum (const 0)
  fromInteger = constant . fromInteger

instance Real a => Real (Diff a) where
  toRational = toRational . getValue

instance Fractional a => Fractional (Diff a) where
  -- d(1/x) = -1 / x^2
  recip = elementary recip (negate . recip . squared)
  fromRational = constant . fromRational

instance Floating a => Floating (Diff a) where
  pi = constant pi
  -- d(exp x) = exp x
  exp = elementary exp exp
  -- d(ln x) = 1/x
  log = elementary log recip
  -- d(sqrt x) = 1 / 2(sqrt x)
  sqrt = elementary sqrt \x -> recip $ 2 * sqrt x
  -- d(sin x) = cos x
  sin = elementary sin cos
  -- d(cos x) = -(sin x)
  cos = elementary cos (negate . sin)
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
  -- d(arcsinh x) = 1 / sqrt(x^2 + 1)
  asinh = elementary asinh \x -> recip $ sqrt $ squared x + 1
  -- d(arccosh x) = 1 / sqrt(x^2 - 1)
  acosh = elementary acosh \x -> recip $ sqrt $ squared x - 1
  -- d(arctanh x) = 1 / (1 - x^2)
  atanh = elementary atanh \x -> recip $ 1 - squared x

diff' :: Num a => (Diff a -> Diff a) -> a -> Diff a
diff' f = f . independent

diff :: Num a => (Diff a -> Diff a) -> a -> a
diff f = getGradient . diff' f

-- >>> diff' sin 0
-- Diff {getGradient = 1.0, getValue = 0.0}

-- >>> diff' exp 0
-- Diff {getGradient = 1.0, getValue = 1.0}

-- >>> diff' (exp . log) 2
-- Diff {getGradient = 1.0, getValue = 2.0}

-- >>> let t = 2.0 in diff (\x -> constant t * sin x) 0
-- 2.0

-- >>> diff (diff (diff sin)) 1
-- -0.5403023058681398

-- >>> diff (\x -> x ** x) 5
-- 8154.493476356562

-- >>> diff (\x -> exp x / x ^ 2) 5
-- 3.5619158184618382
