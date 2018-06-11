{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Safe               #-}

{-|
Module: Data.TypeNums
Copyright: (c) 2018 Iris Ward
License: BSD3
Maintainer: aditu.venyhandottir@gmail.com
Stability: experimental

This module provides a unified interface for natural numbers,
signed integers, and rationals at the type level, in a way fully
compatible with existing code using type-level naturals.

Natural numbers are expressed as always, e.g. @5@.  Negative integers
are written as @Neg 3@.  Ratios are written as @3 :% 2@.
-}
module Data.TypeNums
  ( -- * Type level numbers
    -- ** Naturals
    Nat
  , KnownNat
  , natVal
  , natVal'
    -- ** Integers
  , NInt(Neg)
  , KnownInt
  , intVal
  , intVal'
    -- ** Rationals
  , Rat((:%))
  , KnownRat
  , ratVal
  , ratVal'
    -- * Type level numerical operations
    -- ** Comparisons
  , type (==)
  , type (/=)
  , type (<=)
  , type (<)
  , type (>=)
  , type (>)
    -- ** Arithmetic
  , type (*)
  ) where

import Data.TypeInts
import Data.TypeNums.Equality
import Data.TypeNums.Internal
import Data.TypeRats
import GHC.TypeLits           (KnownNat, Nat, natVal, natVal')
