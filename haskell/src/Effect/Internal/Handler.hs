{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
module Effect.Internal.Handler where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT (runReaderT))
import qualified Control.Monad.Reader   as MTL
import qualified Data.TypeRepMap        as TMap
import           Effect.Internal.Monad
import           System.IO.Unsafe       (unsafePerformIO)
import           Unsafe.Coerce          (unsafeCoerce)

interpret :: Legal e => Handler (e ': es) e -> Eff (e ': es) a -> Eff es a
interpret f m = PrimEff do
  hdl <- MTL.ask
  liftIO $ runReaderT (primRunEff m) (TMap.insert (HandlerOf f) (unsafeCoerce hdl))

interpose :: e :> es => Handler es e -> Eff es a -> Eff es a
interpose f m = PrimEff do
  hdl <- MTL.ask
  liftIO $ runReaderT (primRunEff m) (TMap.insert (HandlerOf f) hdl)

runPure :: Eff '[] a -> a
runPure = unsafePerformIO . (`runReaderT` TMap.empty) . primRunEff
