{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Effect.Reader where

import           Data.Typeable (Typeable)
import           Effect

data Reader r :: Effect where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

runReader :: forall r es a. Typeable r => r -> Eff (Reader r ': es) a -> Eff es a
runReader r = interpretH (h r)
  where
    h :: forall es'. r -> HandlerH es' (Reader r)
    h x run = \case
      Ask        -> pure x
      Local f m' -> run $ interposeH (h (f x)) m'
