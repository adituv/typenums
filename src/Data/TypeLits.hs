{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Safe               #-}

{-|
Module: Data.TypeLits
Copyright: (c) 2018 Iris Ward
License: BSD3
Maintainer: aditu.venyhandottir@gmail.com
Stability: experimental

This module provides the same interface as "GHC.TypeLits", but with
naming conflicts resolved in favour of this package.  For example,
'(<=)' resolves to the kind-polymorphic version from "Data.TypeNums".

If you are only working with type-level numbers, import "Data.TypeNums"
instead.  This module is purely for convenience for those who want to
use both functionality from "GHC.TypeLits" and functionality from
"Data.TypeNums".
-}

module Data.TypeLits
  (
    -- * Type level numbers
    -- ** Naturals
    G.Nat
  , G.KnownNat
  , G.natVal
  , G.natVal'
  , G.SomeNat(..)
  , G.someNatVal
  , G.sameNat

    -- ** Integers
  , TInt(..)
  , KnownInt
  , intVal
  , intVal'
    -- ** Rationals
  , Rat((:%))
  , KnownRat
  , ratVal
  , ratVal'
    -- * Type level numerical operations
    -- ** Comparison
  , type (==?)
  , type (/=?)
  , type (<=?)
  , type (==)
  , type (/=)
  , type (<=)
  , type (<)
  , type (>=)
  , type (>)
    -- ** Arithmetic
  , type (+)
  , type (-)
  , type (*)
  , type (G.^)

    -- * Symbols
  , G.Symbol
#if MIN_VERSION_base(4,10,0)
  , G.AppendSymbol
#endif
  , G.CmpSymbol
  , G.KnownSymbol
  , G.symbolVal
  , G.symbolVal'
  , G.SomeSymbol(..)
  , G.someSymbolVal
  , G.sameSymbol

    -- * User-defined type errors
  , G.TypeError
  , G.ErrorMessage(..)

  ) where

import Data.TypeNums
import qualified GHC.TypeLits as G
