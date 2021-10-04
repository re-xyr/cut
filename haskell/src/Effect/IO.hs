{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Effect.IO where

import           Control.Monad.IO.Class
import           Effect
import           Effect.Internal.Monad
import           Effect.Primitive.IO
import           UnliftIO

data IOE :: Effect where
  Lift :: IO a -> IOE m a
  Unlift :: ((forall x. m x -> IO x) -> IO a) -> IOE m a

instance IOE :> es => MonadIO (Eff es) where
  liftIO = send . Lift
  {-# INLINE liftIO #-}

instance IOE :> es => MonadUnliftIO (Eff es) where
  withRunInIO f = send $ Unlift f
  {-# INLINE withRunInIO #-}

runIOE :: Eff '[IOE] a -> IO a
runIOE = (`primRunEff` emptyEnv) . interpret \case
  Lift m   -> primLiftIO m
  Unlift f -> primUnliftIO \runInIO -> f (runInIO . unlift)
