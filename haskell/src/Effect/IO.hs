{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Effect.IO where

import qualified Data.TypeRepMap       as TMap
import           Effect
import           Effect.Internal.Monad
import           Effect.Prim.IO

data IOE :: Effect where
  Lift :: IO a -> IOE m a
  Unlift :: ((forall x. m x -> IO x) -> IO a) -> IOE m a

runIOE :: Eff '[IOE] a -> IO a
runIOE = (`primRunEff` TMap.empty) . interpret \case
  Lift m   -> primLiftIO m
  Unlift f -> primUnliftIO \runInIO -> f (runInIO . unlift)
