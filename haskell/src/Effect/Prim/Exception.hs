module Effect.Prim.Exception where

import           Effect.Internal.Monad
import           UnliftIO.Exception

primThrow :: Exception e => e -> Eff es a
primThrow e = PrimEff $ throwIO e
{-# INLINE primThrow #-}

primCatch :: Exception e => Eff es a -> (e -> Eff es a) -> Eff es a
primCatch m h = PrimEff $ catch (primRunEff m) (primRunEff . h)
{-# INLINE primCatch #-}

primTry :: Exception e => Eff es a -> Eff es (Either e a)
primTry m = PrimEff $ try $ primRunEff m
{-# INLINE primTry #-}

primBracket :: Eff es a -> (a -> Eff es b) -> (a -> Eff es c) -> Eff es c
primBracket a r m = PrimEff $ bracket (primRunEff a) (primRunEff . r) (primRunEff . m)
{-# INLINE primBracket #-}
