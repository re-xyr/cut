{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE TypeFamilies       #-}
module Autodiff.Forward where

import           Autodiff
import           Data.Functor.Compose (Compose (Compose, getCompose))
import           Data.IntMap.Lazy     (IntMap)
import qualified Data.IntMap.Lazy     as Map
import           Data.Number.Erf      (Erf, InvErf)
import           Data.Traversable     (mapAccumL)
import qualified Debug.SimpleReflect  as Reflect

data Diff a = Diff
  { getGradient :: !(IntMap a) -- Map of partial derivatives (n -> ∂u/∂xn)
  , getValue    :: !a
  } deriving (Show)
    deriving (Eq, Ord, Enum, Bounded, Num, Real, Fractional, Floating, Erf, InvErf)
      via (ByAutodiff (Diff a))

instance Num a => Autodiff (Diff a) where
  type Scalar _ = a
  -- ∀n, ∂C/∂xn = 0
  constant = Diff Map.empty
  {-# INLINE constant #-}
  value = getValue
  {-# INLINE value #-}
  -- ∀n, ∂f(u)/∂xn = ∂f(u)/∂u * ∂u/∂xn
  elementary f f' (Diff dy y) = Diff ((f' y *) <$> dy) (f y)
  {-# INLINE elementary #-}
  -- ∀n, ∂f(u, v)/∂xn = ∂f(u, v)/∂u * ∂u/∂xn + ∂f(u, v)/∂v * ∂v/∂xn
  arithmetic f f' (Diff du u) (Diff dv v) = let (dx, dy) = f' u v in
    Diff (Map.unionWith (+) ((dx *) <$> du) ((dy *) <$> dv)) (f u v)
  {-# INLINE arithmetic #-}
  -- generally, ∀n, ∂f(u1, ..., um)/∂xn = ∂f(u1, ..., um)/∂u1 * ∂u1/∂xn + ... + ∂f(u1, ..., um)/∂um * ∂um/∂xn

instance (Num a, Eq a) => Constable (Diff a) where
  isConstant (Diff m x) = if Map.null m then Just x else Nothing

getTangent :: Num a => Int -> IntMap a -> a
getTangent = Map.findWithDefault 0

mapWithIndexL :: Traversable f => (Int -> a -> b) -> f a -> f b
mapWithIndexL f xs = snd $ mapAccumL (\n x -> (n + 1, f n x)) 0 xs

-- dx/dx = 1
independent :: Num a => a -> Diff a
independent = Diff (Map.singleton 0 1)
{-# INLINE independent #-}

independentN :: Num a => Int -> a -> Diff a
independentN n = Diff (Map.singleton n 1)
{-# INLINE independentN #-}

-- ∀n, ∂xn/∂xn = 1
independents :: (Num a, Traversable f) => f a -> f (Diff a)
independents = mapWithIndexL independentN

-- df(x)/dx = ∂f(x)/∂x
diff :: Num a => (Diff a -> Diff a) -> a -> a
diff f x = getTangent 0 . getGradient . f $ independent x
{-# INLINE diff #-}

-- >>> let t = 2.0 in diff (\x -> constant t * sin x) 0
-- >>> diff (diff (diff sin)) 1
-- >>> diff (\x -> x ** x) 5
-- 2.0
-- -0.5403023058681398
-- 8154.493476356563

-- >>> diff (\x -> exp x / x ^ 2) Reflect.x
-- >>> diff (logBase 10) Reflect.x
-- recip (x * x) * (exp x * 1) + negate (exp x / (x * x * (x * x))) * (x * 1 + x * 1)
-- recip (x * log 10) * 1

diffVec :: (Num a, Functor f) => (Diff a -> f (Diff a)) -> a -> f a
diffVec f x = getTangent 0 . getGradient <$> f (independent x)
{-# INLINE diffVec #-}

-- >>> diffVec (\a -> [sin a, cos a]) 0
-- [1.0,-0.0]

partial :: (Num a, Traversable f) => (f (Diff a) -> Diff a) -> Int -> f a -> a
partial f n xs = getTangent n . getGradient . f $ independents xs
{-# INLINE partial #-}

fromMap :: (Num a, Traversable f) => f a -> IntMap a -> f a
fromMap xs res = mapWithIndexL (\n _ -> getTangent n res) xs

partials :: (Num a, Traversable f) => (f (Diff a) -> Diff a) -> f a -> f a
partials f xs = fromMap xs . getGradient . f $ independents xs
{-# INLINE partials #-}

-- >>> partials (\[x, y] -> x ** y) [0, 2]
-- >>> partials (\[x, y, z] -> x * y + z) [1, 2, 3]
-- >>> partials (\[x,y,z] -> x * sin (x + log y)) [Reflect.x, Reflect.y, Reflect.z]
-- [0.0,NaN]
-- [2,1,1]
-- [sin (x + log y) * 1 + x * (cos (x + log y) * (1 * 1)),x * (cos (x + log y) * (1 * (recip y * 1))),0]

jacobian :: (Num a, Traversable f, Functor g) => (f (Diff a) -> g (Diff a)) -> f a -> g (f a)
jacobian f xs = fromMap xs . getGradient <$> f (independents xs)
{-# INLINE jacobian #-}

-- >>> jacobian (\[x, y] -> [y, x, x + y, x * y, exp x * sin y]) [pi, 1]
-- [[0.0,1.0],[1.0,0.0],[1.0,1.0],[1.0,3.141592653589793],[19.472221418841606,12.502969588876512]]

hessian :: (Traversable f, Num a) => (f (Diff (Diff a)) -> Diff (Diff a)) -> f a -> f (f a)
hessian f = jacobian (partials f)
{-# INLINE hessian #-}

-- >>> hessian (\[x, y] -> x * y) [1, 2]
-- [[0,1],[1,0]]

hessianVec :: (Num a, Traversable f, Functor g) => (f (Diff (Diff a)) -> g (Diff (Diff a))) -> f a -> g (f (f a))
hessianVec f = getCompose . jacobian (Compose . jacobian f)
{-# INLINE hessianVec #-}

-- >>> hessianVec (\[x, y] -> [x * y, x + y, exp x * cos y]) [1, 2]
-- [[[0.0,1.0],[1.0,0.0]],[[0.0,0.0],[0.0,0.0]],[[-1.1312043837568135,-2.4717266720048188],[-2.4717266720048188,1.1312043837568135]]]

diffDir :: (Num a, Traversable f) => (f (Diff a) -> Diff a) -> f (a, a) -> a
diffDir f xs = sum . Map.elems . getGradient . f $ mapWithIndexL (\n (x, dx) -> Diff (Map.singleton n dx) x) xs
{-# INLINE diffDir #-}

diffDirVec :: (Num a, Traversable f, Functor g) => (f (Diff a) -> g (Diff a)) -> f (a, a) -> g a
diffDirVec f xs = sum . Map.elems . getGradient <$> f (mapWithIndexL (\n (x, dx) -> Diff (Map.singleton n dx) x) xs)
{-# INLINE diffDirVec #-}

hessianProduct :: (Num a, Traversable f) => (f (Diff (Diff a)) -> Diff (Diff a)) -> f (a, a) -> f a
hessianProduct f = diffDirVec (partials f)
{-# INLINE hessianProduct #-}
