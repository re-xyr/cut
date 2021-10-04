{-# LANGUAGE BlockArguments #-}
module Effect.Prim.Exception where

import           Control.Exception
import           Effect.Internal.Monad
import           Effect.Prim.IO

primThrow :: Exception e => e -> Eff es a
primThrow e = primLiftIO $ throwIO e
{-# INLINE primThrow #-}

primCatch :: Exception e => Eff es a -> (e -> Eff es a) -> Eff es a
primCatch m h = primUnliftIO \unlift -> catch (unlift m) (unlift . h)
{-# INLINE primCatch #-}

primTry :: Exception e => Eff es a -> Eff es (Either e a)
primTry m = primUnliftIO \unlift -> try (unlift m)
{-# INLINE primTry #-}

primBracket :: Eff es a -> (a -> Eff es b) -> (a -> Eff es c) -> Eff es c
primBracket a r m = primUnliftIO \unlift -> bracket (unlift a) (unlift . r) (unlift . m)
{-# INLINE primBracket #-}
