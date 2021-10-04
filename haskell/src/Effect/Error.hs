{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Effect.Error where

import           Control.Exception          (Exception)
import           Effect
import           Effect.Primitive.Exception

data Error e :: Effect where
  ThrowError :: e -> Error e m a
  CatchError :: m a -> (e -> m a) -> Error e m a

throwError :: Error e :> es => e -> Eff es a
throwError e = send $ ThrowError e
{-# INLINE throwError #-}

catchError :: Error e :> es => Eff es a -> (e -> Eff es a) -> Eff es a
catchError m h = send $ CatchError m h
{-# INLINE catchError #-}

tryError :: Error e :> es => Eff es a -> Eff es (Either e a)
tryError m = (Right <$> m) `catchError` (pure . Left)
{-# INLINE tryError #-}

runError :: forall e es a. (Exception e) => Eff (Error e ': es) a -> Eff es (Either e a)
runError = primTry . interpret \case
  ThrowError e     -> primThrow e
  CatchError m' h' -> primCatch (unlift m') (unlift . h')
