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

import           Control.Concurrent.MVar    (MVar)
import           Data.IORef                 (IORef)
import           Data.Typeable              (Typeable)
import           Effect
import           Effect.Primitive.Exception (primBracket)
import           Effect.Primitive.IORef     (primModifyIORef', primNewIORef,
                                             primReadIORef)
import           Effect.Primitive.MVar      (primModifyMVarPure'_, primNewMVar,
                                             primReadMVar)

data Writer w :: Effect where
  Tell :: w -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

tell :: Writer w :> es => w -> Eff es ()
tell w = send $ Tell w
{-# INLINE tell #-}

listen :: Writer w :> es => Eff es a -> Eff es (a, w)
listen m = send $ Listen m
{-# INLINE listen #-}

listens :: Writer w :> es => (w -> x) -> Eff es a -> Eff es (a, x)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)

runLocalWriter :: forall w es a. (Typeable w, Monoid w) => Eff (Writer w ': es) a -> Eff es (a, w)
runLocalWriter m = do
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

runSharedWriter :: forall w es a. (Typeable w, Monoid w) => Eff (Writer w ': es) a -> Eff es (a, w)
runSharedWriter m = do
  rw <- primNewMVar mempty
  x <- interpret (h rw) m
  w' <- primReadMVar rw
  pure (x, w')
  where
    h :: forall es'. MVar w -> Handler es' (Writer w)
    h rw = \case
      Tell w'   -> primModifyMVarPure'_ rw (<> w')
      Listen m' -> primBracket (primNewMVar mempty) write run
        where
          write rw' = do
            w' <- primReadMVar rw'
            primModifyMVarPure'_ rw (<> w')
          run rw' = do
            x <- unlift $ interpose (h rw') m'
            w' <- primReadMVar rw'
            pure (x, w')
