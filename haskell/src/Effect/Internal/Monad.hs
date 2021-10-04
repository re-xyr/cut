{-# LANGUAGE BlockArguments             #-}
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

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, runReaderT)
import qualified Control.Monad.Reader   as MTL
import           Data.Coerce            (Coercible)
import           Data.Maybe             (fromJust)
import           Data.TypeRepMap        (TypeRepMap)
import qualified Data.TypeRepMap        as TMap
import           Data.Typeable          (Typeable)
import           Unsafe.Coerce          (unsafeCoerce)

type Effect = (* -> *) -> * -> *

type HandlerH es e = forall es' a. e :> es' => (forall x. Eff es' x -> Eff es x) -> e (Eff es') a -> Eff es a

type HandlerF es e = forall es' a. e :> es' => e (Eff es') a -> Eff es a

type Handler es e = forall es' a. e :> es' => Env es' -> e (Eff es') a -> Eff es a

data HandlerOf es e = Representational e => HandlerOf
  { getEnv     :: Env es
  , runHandler :: Handler es e
  }

type Env es = TypeRepMap (HandlerOf es)

newtype Eff (es :: [Effect]) a = PrimEff { primRunEff :: ReaderT (Env es) IO a }
  deriving (Functor, Applicative, Monad)

class (forall m n a. Coercible m n => Coercible (e m a) (e n a)) => Representational e
instance (forall m n a. Coercible m n => Coercible (e m a) (e n a)) => Representational e
type Legal e = (Representational e, Typeable e)

class Legal e => (e :: Effect) :> (es :: [Effect])
instance {-# OVERLAPPING #-} Legal e => e :> (e ': es)
instance e :> es => e :> (f ': es)

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

send :: forall e es a. e :> es => e (Eff es) a -> Eff es a
send e = PrimEff do
  hdls <- MTL.ask
  -- liftIO $ putStrLn $ "send with " ++ show hdls
  let hdl = fromJust $ TMap.lookup hdls
  liftIO $ runReaderT (primRunEff (runHandler hdl hdls e)) (getEnv hdl)

raise :: forall e es a. Eff es a -> Eff (e ': es) a
raise = unsafeCoerce
{-# INLINE raise #-}

subsume :: e :> es => Eff (e ': es) a -> Eff es a
subsume = unsafeCoerce
{-# INLINE subsume #-}
