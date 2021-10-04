{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Effect.Reader where

import           Data.Typeable (Typeable)
import           Effect

data Reader r :: Effect where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

ask :: Reader r :> es => Eff es r
ask = send Ask
{-# INLINE ask #-}

asks :: Reader r :> es => (r -> s) -> Eff es s
asks f = f <$> ask
{-# INLINE asks #-}

local :: Reader r :> es => (r -> r) -> Eff es a -> Eff es a
local f m = send $ Local f m
{-# INLINE local #-}

runReader :: forall r es a. Typeable r => r -> Eff (Reader r ': es) a -> Eff es a
runReader r = interpret (h r)
  where
    h :: forall es'. r -> Handler es' (Reader r)
    h x = \case
      Ask        -> pure x
      Local f m' -> unlift $ interpose (h (f x)) m'
