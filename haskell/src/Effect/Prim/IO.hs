{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
module Effect.Prim.IO where

import           Effect.Internal.Monad
import           UnliftIO

primLiftIO :: IO a -> Eff es a
primLiftIO m = PrimEff $ liftIO m
{-# INLINE primLiftIO #-}

primUnliftIO :: ((forall x. Eff es x -> IO x) -> IO a) -> Eff es a
primUnliftIO f = PrimEff $ withRunInIO \unlift -> f (unlift . primRunEff)
{-# INLINE primUnliftIO #-}
