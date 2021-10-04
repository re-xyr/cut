{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Effect.Internal.Monad where

import           Control.Monad.Fix (MonadFix (mfix))
import           Data.Coerce       (Coercible)
import           Data.Maybe        (fromJust)
import           Data.Reflection   (Given, give)
import           Data.TypeRepMap   (TypeRepMap)
import qualified Data.TypeRepMap   as TMap
import           Data.Typeable     (Typeable)
import           Unsafe.Coerce     (unsafeCoerce)

type Effect = (* -> *) -> * -> *

type Handler es e = forall es' a. (Given (Env es'), e :> es') => e (Eff es') a -> Eff es a

data HandlerOf es e = Representational e => HandlerOf
  { getEnv     :: Env es
  , runHandler :: Handler es e
  }

type Env es = TypeRepMap (HandlerOf es)

newtype Eff (es :: [Effect]) a = PrimEff { primRunEff :: Env es -> IO a }
  deriving (Monoid, Semigroup)

instance Functor (Eff es) where
  fmap f (PrimEff m) = PrimEff (fmap f . m)
  a <$ PrimEff b = PrimEff \es -> a <$ b es

instance Applicative (Eff es) where
  pure x = PrimEff \_ -> pure x
  PrimEff mf <*> PrimEff mx = PrimEff \es -> mf es <*> mx es
  PrimEff ma  *> PrimEff mb = PrimEff \es -> ma es  *> mb es
  PrimEff ma <*  PrimEff mb = PrimEff \es -> ma es <*  mb es

instance Monad (Eff es) where
  PrimEff m >>= k = PrimEff \es -> m es >>= \a -> primRunEff (k a) es
  PrimEff ma >> PrimEff mb = PrimEff \es -> ma es >> mb es

instance MonadFix (Eff es) where
  mfix f = PrimEff \es -> mfix \a -> primRunEff (f a) es

class (forall m n a. Coercible m n => Coercible (e m a) (e n a)) => Representational e
instance (forall m n a. Coercible m n => Coercible (e m a) (e n a)) => Representational e
type Legal e = (Representational e, Typeable e)

class Legal e => (e :: Effect) :> (es :: [Effect]) where
  inst :: ()
  inst = ()
instance {-# OVERLAPPING #-} Legal e => e :> (e ': es)
instance e :> es => e :> (f ': es)

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

send :: forall e es a. e :> es => e (Eff es) a -> Eff es a
send e = PrimEff \hdls ->
  let hdl = fromJust $ TMap.lookup hdls
  in primRunEff ((give hdls $ runHandler hdl) e) (getEnv hdl)

raise :: forall e es a. Eff es a -> Eff (e ': es) a
raise = unsafeCoerce
{-# INLINE raise #-}

subsume :: e :> es => Eff (e ': es) a -> Eff es a
subsume = unsafeCoerce
{-# INLINE subsume #-}
