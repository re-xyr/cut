{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Effect.Timeout where

import           Effect
import           Effect.IO
import qualified UnliftIO.Timeout as T

data Timeout :: Effect where
  Timeout :: Int -> m a -> Timeout m (Maybe a)

runTimeout :: IOE :> es => Eff (Timeout ': es) a -> Eff es a
runTimeout = interpret \case
  Timeout n m -> T.timeout n (unlift m)

timeout :: Timeout :> es => Int -> Eff es a -> Eff es (Maybe a)
timeout n m = send $ Timeout n m
{-# INLINE timeout #-}
