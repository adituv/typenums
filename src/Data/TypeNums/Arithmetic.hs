{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE Safe               #-}
{-# LANGUAGE TypeOperators      #-}

-- | This module provides operators for type-level arithmetic.  To
--   extend this functionality, add new type instances to the underlying
--   type families found in "Data.TypeNums.Arithmetic.Internal".
module Data.TypeNums.Arithmetic
  ( type (+)
  , type (-)
  , type (*)
  ) where

import Data.TypeNums.Arithmetic.Internal

infixl 6 +, -
infixl 7 *

-- | The sum of two type-level numbers
type (+) a b = Add a b

-- | The difference of two type-level numbers
type (-) a b = Sub a b

-- | The product of two type-level numbers
type (*) a b = Mul a b
