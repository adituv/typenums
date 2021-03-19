{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE Safe               #-}
{-# LANGUAGE TypeOperators      #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType #-}
#endif

-- | This module provides operators for type-level arithmetic.  To
--   extend this functionality, add new type instances to the underlying
--   type families found in "Data.TypeNums.Arithmetic.Internal".
module Data.TypeNums.Arithmetic
  ( type (+)
  , type (-)
  , type (*)
  , type (/)
  , DivMod
  , QuotRem
  , Div
  , Mod
  , Quot
  , Rem
  , Abs
  , Negate
  , Recip
  , GCD
  , Simplify
  ) where

import Data.TypeNums.Arithmetic.Internal

infixl 6 +, -
infixl 7 *

-- | The sum of two type-level numbers
type (+) a b = Add a b

-- | The difference of two type-level numbers
type (-) a b = Sub a b

-- | The product of two type-level numbers.
--
--   Due to changes in GHC 8.6, using this operator infix and unqualified
--   requires the NoStarIsType language extension to be active.  See the GHC
--   8.6.x migration guide for details:
--   <https://ghc.haskell.org/trac/ghc/wiki/Migration/8.6>
type (*) a b = Mul a b

-- | The ratio of two type-level numbers
type (/) a b = RatDiv a b
