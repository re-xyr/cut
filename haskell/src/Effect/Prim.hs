{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Effect.Prim where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Primitive (PrimMonad (..))
import           Effect
import           Effect.IO               (IOE)
import           GHC.Exts                (RealWorld, State#)
import           GHC.IO                  (IO (IO))

data Prim :: Effect where
  Primitive :: (State# RealWorld -> (# State# RealWorld, a #)) -> Prim m a

instance Prim :> es => PrimMonad (Eff es) where
  type PrimState (Eff es) = RealWorld
  primitive = send . Primitive
  {-# INLINE primitive #-}

runPrim :: IOE :> es => Eff (Prim ': es) a -> Eff es a
runPrim = interpret \case
  Primitive m -> liftIO (IO m)
