{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}
module Effect.Fresh where

import           Effect
import           Effect.State

data Fresh :: Effect where
  Fresh :: Fresh m Int

fresh :: Fresh :> es => Eff es Int
fresh = send Fresh
{-# INLINE fresh #-}

runLocalFresh :: Int -> Eff (Fresh ': es) a -> Eff es (a, Int)
runLocalFresh n = runLocalState n . reinterpret \case
  Fresh -> state \s -> (s, s + 1)
