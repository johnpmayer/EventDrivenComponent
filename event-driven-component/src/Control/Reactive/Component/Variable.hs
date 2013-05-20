{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, ScopedTypeVariables, TypeOperators #-}

module Data.Reactive.Component.Variable where

import GHC.TypeLits

data (:::) :: Symbol -> * -> * where
  Variable :: sy ::: t



