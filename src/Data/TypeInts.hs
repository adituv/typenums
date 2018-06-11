{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module: Data.TypeInts
Copyright: (c) 2018 Iris Ward
License: BSD3
Maintainer: aditu.venyhandottir@gmail.com
Stability: experimental

Type level integers can be used in the same way as type level naturals
from "GHC.TypeLits", for example @3@.  However, a minus sign is not
recognised in this context, so a negative type-level integer is instead
written as @'Neg' n@.
-}
module Data.TypeInts
  ( -- * Construction
    NInt(Neg)
    -- * Linking type and value level
  , KnownInt
  , intVal
  , intVal'
  ) where

import GHC.Exts     (Proxy#, proxy#)
import GHC.TypeLits (KnownNat, Nat, natVal')

newtype SInt (n :: k) =
  SInt Integer

-- | (Kind) A negative integer
newtype NInt =
  Neg Nat

-- | This class gives the (value-level) integer associated with a type-level
--   integer.  There are instances of this class for every concrete natural:
--   0, 1, 2, etc.  There are also instances of this class for every negated
--   natural, such as @'Neg' 1@.
class KnownInt (n :: k) where
  intSing :: SInt n

instance forall n. KnownNat n => KnownInt n where
  intSing = SInt $! natVal' (proxy# @Nat @n)

instance forall n. KnownNat n => KnownInt ('Neg n) where
  intSing = SInt $! negate (natVal' (proxy# @Nat @n))

-- | Get the value associated with a type-level integer
intVal ::
     forall n proxy. KnownInt n
  => proxy n
  -> Integer
intVal _ =
  case intSing :: SInt n of
    SInt x -> x

-- | Get the value associated with a type-level integer.  The difference
--   between this function and 'intVal' is that it takes a 'Proxy#' parameter,
--   which has zero runtime representation and so is entirely free.
intVal' ::
     forall n. KnownInt n
  => Proxy# n
  -> Integer
intVal' _ =
  case intSing :: SInt n of
    SInt x -> x
