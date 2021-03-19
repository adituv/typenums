{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType #-}
#endif

module Data.TypeNums.Comparison where

import Data.TypeNums.Arithmetic
import Data.TypeNums.Ints
import Data.TypeNums.Rats
import GHC.TypeLits             (Nat)

import qualified GHC.TypeLits as G

infix 4 <=?, <=, <, >=, >

-- | Boolean comparison of two type-level numbers
type family (a :: k1) <=? (b :: k2) :: Bool where
  (a :: Nat)  <=? (b :: Nat)  = (G.<=?) a b
  'Pos a      <=? 'Pos b      = (G.<=?) a b
  'Neg _      <=? 'Pos _      = 'True
  'Pos _      <=? 'Neg _      = 'False
  'Pos a      <=? b           = a <=? b -- unwrap Nat from Pos
  a           <=? 'Pos b      = a <=? b -- unwrap Nat from Pos
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
