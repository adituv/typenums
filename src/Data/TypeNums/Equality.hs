{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeNums.Equality where

import           Data.Type.Bool     (Not)
import qualified Data.Type.Equality as DTE
import           Data.TypeNums.Arithmetic.Internal(Simplify)
import           Data.TypeNums.Rats.Type(Rat(..))

infix 4 ==?, /=?, ==, /=

-- | Boolean type-level equals.  Useful for e.g.
--   @'Data.Type.Bool.If' (x ==? 0)@
type family (a :: k) ==? (b :: k) :: Bool where
  (a :: Rat) ==? (b :: Rat) = (DTE.==) (Simplify a) (Simplify b)
  a          ==? b          = (DTE.==) a b

-- | Boolean type-level not-equals.
type (a :: k) /=? (b :: k) = Not ((DTE.==) a b)

-- | Equality constraint, used as e.g. @(x == 3) => _@
type (a :: k) == (b :: k) = ((DTE.==) a b) ~ 'True

-- | Not-equal constraint
type (a :: k) /= (b :: k) = ((DTE.==) a b) ~ 'False
