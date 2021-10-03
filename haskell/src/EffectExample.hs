{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}
module EffectExample where

import           Control.Monad (unless)
import           Data.Monoid   (Sum (Sum))
import           Effect
import           Effect.IO
import           Effect.Reader
import           Effect.State
import           Effect.Writer

data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

runTeletypeIO :: IOE :> es => Eff (Teletype ': es) a -> Eff es a
runTeletypeIO = interpret \case
  ReadTTY    -> send (Lift getLine)
  WriteTTY s -> send (Lift $ putStrLn s)

runTeletypePure :: (State [String] :> es, Writer [String] :> es) => Eff (Teletype ': es) w -> Eff es w
runTeletypePure = interpret \case
  ReadTTY -> send Get >>= \case
    []     -> pure ""
    x : xs -> send (Put xs) >> pure x
  WriteTTY msg -> send (Tell [msg])

echo :: Teletype :> es => Eff es ()
echo = do
  x <- send ReadTTY
  unless (null x) $ do
    send $ WriteTTY x
    echo

test :: [String] -> [String]
test xs = snd $ runPure $ runWriterByIORef $ runStateByIORef xs $ runTeletypePure echo


wowwee :: (Reader Integer :> es, Writer (Sum Integer) :> es) => Eff es ()
wowwee = do
  n <- send Ask
  if n == (0 :: Integer) then pure ()
  else do
    send $ Tell (Sum n)
    send $ Local (subtract (1 :: Integer)) wowwee

-- >>> runPure $ runReader 100 $ runWriterByIORef @(Sum Integer) wowwee
