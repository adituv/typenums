{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: Data.TypeNums.Internal
Copyright: (c) 2018 Iris Ward
License: BSD3
Maintainer: aditu.venyhandottir@gmail.com
Stability: experimental

Internal details of type-level numbers, currently comprising infrastructure
needed to compare mixed kinds of types.
-}
module Data.TypeNums.Internal where

import Data.TypeInts
import Data.TypeRats
import GHC.TypeLits  (Nat)

import qualified GHC.TypeLits as G

-- TODO: Reduce rationals.  Seems to require GHC 8.4, so probably needs CPP magic
-- TODO: More arithmetic.  Addition and subtraction harder because not fixed kind

-- | The kind of the result of multiplication
type family MulK k1 k2 where
  MulK Nat  Nat  = Nat
  MulK NInt NInt = Nat
  MulK Nat  NInt = NInt
  MulK NInt Nat  = NInt
  MulK a    Rat  = Rat
  MulK Rat   a   = Rat

-- | The product of two type-level numbers
type family (x :: k1) * (y :: k2) :: MulK k1 k2 where
  (x :: Nat)  * (y :: Nat)  = (G.*) x y
  'Neg x      * 'Neg y      = (G.*) x y
  'Neg x      * (y :: Nat)  = 'Neg ((G.*) x y)
  (x :: Nat)  * 'Neg y      = 'Neg ((G.*) x y)
  (n1 ':% d1) * (n2 ':% d2) = (n1 * n2) ':% (d1 * d2)
  (n ':% d)   * y           = (n * y) ':% d
  x           * (n ':% d)   = (x * n) ':% d

-- | Boolean comparison of two type-level numbers
type family (a :: k1) <=? (b :: k2) :: Bool where
  (a :: Nat)  <=? (b :: Nat)  = (G.<=?) a b
  0           <=? 'Neg 0      = 'True
  'Neg a      <=? (b :: Nat)  = 'True
  'Neg a      <=? 'Neg b      = (G.<=?) b a
  (n1 ':% d1) <=? (n2 ':% d2) = (n1 * d1) <=? (n2 * d2)
  a           <=? (n ':% d)   = (a * d) <=? n
  (n ':% d)   <=? b           = n <=? (b * d)

type (a :: k1) <= (b :: k2) = (a <=? b) ~ 'True
type (a :: k1) <  (b :: k2) = (b <=? a) ~ 'False
type (a :: k1) >= (b :: k2) = (b <=? a) ~ 'True
type (a :: k1) >  (b :: k2) = (a <=? b) ~ 'False
