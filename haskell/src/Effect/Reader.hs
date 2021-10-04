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
runReader r = interpret (h r)
  where
    h :: forall es'. r -> Handler es' (Reader r)
    h x = \case
      Ask        -> pure x
      Local f m' -> unlift $ interpose (h (f x)) m'
