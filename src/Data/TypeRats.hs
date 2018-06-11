{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE Trustworthy               #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

{-|
Module: Data.TypeRats
Copyright: (c) 2018 Iris Ward
License: BSD3
Maintainer: aditu.venyhandottir@gmail.com
Stability: experimental

Type level rational numbers expressed as a ratio between a type-level integer
and a type-level natural.  For example @'Neg' 3 :% 2@.

See also: "Data.TypeInts"
-}
module Data.TypeRats
  ( Rat((:%))
  , KnownRat
  , ratVal
  , ratVal'
  ) where

import Data.TypeInts
import Data.TypeNums.Equality (type (/=))

import Data.Ratio   (Rational, (%))
import GHC.Exts     (Proxy#, proxy#)
import GHC.TypeLits (ErrorMessage (..), KnownNat, Nat, TypeError, natVal')

-- | Type constructor for a rational
data Rat = forall k. k :% Nat

newtype SRat r =
  SRat Rational

-- | This class gives the (value-level) rational associated with a type-level
--   rational.  There are instances of this class for every combination of a
--   concrete integer and concrete natural.
class KnownRat r where
  ratSing :: SRat r

instance {-# OVERLAPPING #-} (TypeError ('Text "Denominator must not equal 0")) =>
                             KnownRat (n ':% 0) where
  ratSing = error "Unreachable"

instance {-# OVERLAPS #-} forall (n :: k) d. (KnownInt n, KnownNat d, d /= 0) =>
                          KnownRat (n ':% d) where
  ratSing = SRat $! intVal' (proxy# @k @n) % natVal' (proxy# @Nat @d)

instance {-# OVERLAPPABLE #-} forall (n :: k). (KnownInt n) => KnownRat n where
  ratSing = SRat $! intVal' (proxy# @k @n) % 1

-- | Get the value associated with a type-level rational
ratVal ::
     forall proxy r. KnownRat r
  => proxy r
  -> Rational
ratVal _ =
  case ratSing :: SRat r of
    SRat x -> x

-- | Get the value associated with a type-level rational.  The difference
--   between this function and 'ratVal' is that it takes a 'Proxy#' parameter,
--   which has zero runtime representation and so is entirely free.
ratVal' ::
     forall r. KnownRat r
  => Proxy# r
  -> Rational
ratVal' _ =
  case ratSing :: SRat r of
    SRat x -> x
