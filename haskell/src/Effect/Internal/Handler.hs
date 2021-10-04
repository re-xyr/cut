{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Effect.Internal.Handler where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT (runReaderT))
import qualified Control.Monad.Reader   as MTL
import qualified Data.TypeRepMap        as TMap
import           Effect.Internal.Monad
import           System.IO.Unsafe       (unsafePerformIO)
import           Unsafe.Coerce          (unsafeCoerce)

interpret :: forall e es a. Legal e => HandlerF es e -> Eff (e ': es) a -> Eff es a
interpret f m = PrimEff do
  hdl <- MTL.ask
  -- liftIO $ putStrLn $ "interpret " ++ show (typeRep (Proxy :: Proxy e)) ++ " with " ++ show hdl
  let inserted = unsafeCoerce $ TMap.insert (HandlerOf hdl (const f)) hdl
  liftIO $ runReaderT (primRunEff m) inserted

interpretH :: forall e es a. Legal e => HandlerH es e -> Eff (e ': es) a -> Eff es a
interpretH f m = PrimEff do
  hdl <- MTL.ask
  -- liftIO $ putStrLn $ "interpret " ++ show (typeRep (Proxy :: Proxy e)) ++ " with " ++ show hdl
  let inserted = unsafeCoerce $ TMap.insert (HandlerOf hdl \env -> f (unlift env)) hdl
  liftIO $ runReaderT (primRunEff m) inserted

interpose :: forall e es a. e :> es => HandlerF es e -> Eff es a -> Eff es a
interpose f m = PrimEff do
  hdl <- MTL.ask
  -- liftIO $ putStrLn $ "interpose " ++ show (typeRep (Proxy :: Proxy e)) ++ " with " ++ show hdl
  let inserted = TMap.insert (HandlerOf hdl (const f)) hdl
  liftIO $ runReaderT (primRunEff m) inserted

interposeH :: forall e es a. e :> es => HandlerH es e -> Eff es a -> Eff es a
interposeH f m = PrimEff do
  hdl <- MTL.ask
  -- liftIO $ putStrLn $ "interpose " ++ show (typeRep (Proxy :: Proxy e)) ++ " with " ++ show hdl
  let inserted = TMap.insert (HandlerOf hdl \env -> f (unlift env)) hdl
  liftIO $ runReaderT (primRunEff m) inserted

runPure :: Eff '[] a -> a
runPure = unsafePerformIO . (`runReaderT` TMap.empty) . primRunEff

unliftIO :: Env es -> Eff es a -> IO a
unliftIO hdl' m = runReaderT (primRunEff m) hdl'
{-# INLINE unliftIO #-}

unlift :: Env es' -> Eff es' a -> Eff es a
unlift hdl' = PrimEff . liftIO . unliftIO hdl'
{-# INLINE unlift #-}
