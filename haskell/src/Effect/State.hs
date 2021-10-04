{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}
module Effect.State where

import           Data.IORef        (IORef)
import           Data.Typeable     (Typeable)
import           Effect
import           Effect.Prim.IORef

data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()

runStateByIORef :: forall s es a. Typeable s => s -> Eff (State s ': es) a -> Eff es (a, s)
runStateByIORef s m =  do
  rs <- primNewIORef s
  x <- interpret (h rs) m
  s' <- primReadIORef rs
  pure (x, s')
  where
    h :: IORef s -> HandlerF es (State s)
    h rs = \case
      Get    -> primReadIORef rs
      Put s' -> primWriteIORef rs s'
