{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators, TypeFamilies #-}

module Data.Reactive.Component.Signal where

import GHC.TypeLits
import Control.Concurrent.MVar

data Signal :: Nat -> * -> * where
  
  -- A signal whose value never changes
  Const :: a -> Signal 0 a

  -- A signal which updates each time a new values is put into the MVar
  Input :: MVar a -> Signal 0 a

  -- Update the output by running a state machine on a saved state
  FoldP :: (i -> s -> (s,o)) -> s -> Signal n i -> Signal (n + 1) o

  -- Applicative
  Apply :: Signal nf (a -> b) -> Signal nv a -> Signal (nf + nv + 1) b
  
  -- Run an action each time the input chnages.
  -- When the action completes, update the output signal
  Async :: (a -> IO b) -> b -> Signal n a -> Signal 0 b

lift :: (a -> b) -> Signal n a -> Signal (n + 1) b
lift f = FoldP (\i -> \s -> (s, f i)) ()

lift2 :: (a -> b -> c) -> Signal n1 a -> Signal n2 b -> Signal ((n1 + 1) + n2 + 1) c
lift2 f sa = Apply $ lift f sa
