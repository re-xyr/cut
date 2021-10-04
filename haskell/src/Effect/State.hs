{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}
module Effect.State where

import           Control.Concurrent.MVar (MVar)
import           Data.IORef              (IORef)
import           Data.Tuple              (swap)
import           Data.Typeable           (Typeable)
import           Effect
import           Effect.Primitive.IORef
import           Effect.Primitive.MVar

data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()
  State :: (s -> (a, s)) -> State s m a
  StateM :: (s -> m (a, s)) -> State s m a

get :: State s :> es => Eff es s
get = send Get
{-# INLINE get #-}

gets :: State s :> es => (s -> t) -> Eff es t
gets f = f <$> get
{-# INLINE gets #-}

put :: State s :> es => s -> Eff es ()
put s = send $ Put s
{-# INLINE put #-}

state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = send $ State f
{-# INLINE state #-}

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (((), ) . f)
{-# INLINE modify #-}

stateM :: State s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM f = send $ StateM f
{-# INLINE stateM #-}

modifyM :: State s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (fmap ((), ) . f)
{-# INLINE modifyM #-}

runLocalState :: forall s es a. Typeable s => s -> Eff (State s ': es) a -> Eff es (a, s)
runLocalState s m = do
  rs <- primNewIORef s
  x <- interpret (h rs) m
  s' <- primReadIORef rs
  pure (x, s')
  where
    h :: IORef s -> Handler es (State s)
    h rs = \case
      Get -> primReadIORef rs
      Put s' -> primWriteIORef rs s'
      State f -> do
        s' <- primReadIORef rs
        let (a, s'') = f s'
        primWriteIORef rs s''
        pure a
      StateM f -> do
        s' <- primReadIORef rs
        (a, s'') <- unlift $ f s'
        primWriteIORef rs s''
        pure a

runSharedState :: forall s es a. Typeable s => s -> Eff (State s ': es) a -> Eff es (a, s)
runSharedState s m = do
  rs <- primNewMVar s
  x <- interpret (h rs) m
  s' <- primReadMVar rs
  pure (x, s')
  where
    h :: MVar s -> Handler es (State s)
    h rs = \case
      Get      -> primReadMVar rs
      Put s'   -> primWriteMVar rs s'
      State f  -> primModifyMVar' rs (pure . swap . f)
      StateM f -> primModifyMVar' rs (unlift . fmap swap . f)
