module Effect.Prim.IORef where

import           Effect.Internal.Monad
import           UnliftIO.IORef

primNewIORef :: a -> Eff es (IORef a)
primNewIORef x = PrimEff $ newIORef x
{-# INLINE primNewIORef #-}

primReadIORef :: IORef a -> Eff es a
primReadIORef r = PrimEff $ readIORef r
{-# INLINE primReadIORef #-}

primWriteIORef :: IORef a -> a -> Eff es ()
primWriteIORef r x = PrimEff $ writeIORef r x
{-# INLINE primWriteIORef #-}

primModifyIORef' :: IORef a -> (a -> a) -> Eff es ()
primModifyIORef' r f = PrimEff $ modifyIORef' r f
{-# INLINE primModifyIORef' #-}
