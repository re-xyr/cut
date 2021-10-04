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
import qualified System.Timeout as T

data Timeout :: Effect where
  Timeout :: Int -> m a -> Timeout m (Maybe a)

runTimeout :: IOE :> es => Eff (Timeout ': es) a -> Eff es a
runTimeout = interpretH \run -> \case
  Timeout n m -> send $ Unlift \runInIO -> T.timeout n $ runInIO $ run m
