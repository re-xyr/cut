{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
module Effect.Error where

import           Control.Exception     (Exception)
import           Data.Typeable         (Typeable)
import           Effect
import           Effect.Prim.Exception

data Error e :: Effect where
  ThrowError :: e -> Error e m a
  CatchError :: m a -> (e -> m a) -> Error e m a

runError :: forall e es a. (Typeable e, Exception e) => Eff (Error e ': es) a -> Eff es (Either e a)
runError = primTry . interpret \case
  ThrowError e     -> primThrow e
  CatchError m' h' -> primCatch m' h'