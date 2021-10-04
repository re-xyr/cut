{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Effect.Writer where

import           Data.IORef            (IORef)
import           Data.Typeable         (Typeable)
import           Effect
import           Effect.Prim.Exception (primBracket)
import           Effect.Prim.IORef

data Writer w :: Effect where
  Tell :: w -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

runWriterByIORef :: forall w es a. (Typeable w, Monoid w) => Eff (Writer w ': es) a -> Eff es (a, w)
runWriterByIORef m = do
  rw <- primNewIORef mempty
  x <- interpret (h rw) m
  w' <- primReadIORef rw
  pure (x, w')
  where
    h :: forall es'. IORef w -> Handler es' (Writer w)
    h rw = \case
      Tell w'   -> primModifyIORef' rw (<> w')
      Listen m' -> primBracket (primNewIORef mempty) write run
        where
          write rw' = do
            w' <- primReadIORef rw'
            primModifyIORef' rw (<> w')
          run rw' = do
            x <- unlift $ interpose (h rw') m'
            w' <- primReadIORef rw'
            pure (x, w')
