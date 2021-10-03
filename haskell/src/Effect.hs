{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Effect (Effect, Handler, Eff, (:>), (:>>), send, interpret, interpose, raise, subsume, runPure) where

import           Data.Kind               (Constraint)
import           Effect.Internal.Handler
import           Effect.Internal.Monad

type family xs :>> ys :: Constraint where
  '[] :>> ys = ()
  (x ': xs) :>> ys = (x :> ys, xs :>> ys)
