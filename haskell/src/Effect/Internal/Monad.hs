{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
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

import           Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as MTL
import           Data.Coerce          (Coercible)
import           Data.Maybe           (fromJust)
import           Data.TypeRepMap      (TypeRepMap)
import qualified Data.TypeRepMap      as TMap
import           Data.Typeable        (Typeable)
import           Unsafe.Coerce        (unsafeCoerce)

type Effect = (* -> *) -> * -> *

type Handler es e = forall a. e (Eff es) a -> Eff es a

data HandlerOf es e = Representational e => HandlerOf { runHandler :: Handler es e }

type Handlers es = TypeRepMap (HandlerOf es)

newtype Eff (es :: [Effect]) a = PrimEff { primRunEff :: ReaderT (Handlers es) IO a }
  deriving (Functor, Applicative, Monad)

class (forall m n a. Coercible m n => Coercible (e m a) (e n a)) => Representational e
instance (forall m n a. Coercible m n => Coercible (e m a) (e n a)) => Representational e
type Legal e = (Representational e, Typeable e)

class Legal e => e :> es
instance {-# OVERLAPPING #-} Legal e => e :> (e ': es)
instance e :> es => e :> (f ': es)

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

send :: forall e es. e :> es => Handler es e
send e = do
  hdl <- PrimEff (MTL.asks (fromJust . TMap.lookup))
  runHandler hdl e

raise :: forall e es a. Eff es a -> Eff (e ': es) a
raise = unsafeCoerce
{-# INLINE raise #-}

subsume :: e :> es => Eff (e ': es) a -> Eff es a
subsume = unsafeCoerce
{-# INLINE subsume #-}
