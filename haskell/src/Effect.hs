{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Effect (Effect, HandlerF, HandlerH,  Eff, Legal, (:>), (:>>), send, interpret, interpretH, interpose, interposeH, raise, subsume, runPure) where

import           Data.Kind               (Constraint)
import           Effect.Internal.Handler
import           Effect.Internal.Monad

type family (xs :: [Effect]) :>> (ys :: [Effect]) :: Constraint where
  '[] :>> ys = ()
  (x ': xs) :>> ys = (x :> ys, xs :>> ys)
