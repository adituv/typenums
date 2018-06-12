{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeNums.Arithmetic
  ( type (+)
  , type (-)
  , type (*)
  ) where

import Data.Type.Bool     (If)
import Data.TypeNums.Ints
import Data.TypeNums.Rats
import GHC.TypeLits       (Nat)

import qualified GHC.TypeLits as G

-- TODO: Reduce type-level rationals?  Probably not 100% necessary as the
-- user should only see them via ratVal which already handles reduction
-- at value level.

-- | The kind of the result of arithmetic
type family ArithK k1 k2 where
  ArithK Nat  Nat  = Nat
  ArithK a    Rat  = Rat
  ArithK Rat  a    = Rat
  ArithK TInt a    = TInt
  ArithK a    TInt = TInt

-- | The sum of two type-level numbers
type family (x :: k1) + (y :: k2) :: ArithK k1 k2 where
  (x :: Nat)  + (y :: Nat)  = (G.+) x y

  'Pos x      + 'Pos y      = 'Pos ((G.+) x y)
  'Neg x      + 'Pos y      = If ((G.<=?) x y)
                                ('Pos ((G.-) y x))
                                ('Neg ((G.-) x y))
  'Pos x      + 'Neg y      = If ((G.<=?) y x)
                                ('Pos ((G.-) x y))
                                ('Neg ((G.-) y x))
  'Neg x      + 'Neg y      = 'Neg ((G.+) x y)

  'Pos x      + (y :: Nat)  = 'Pos ((G.+) x y)
  'Neg x      + (y :: Nat)  = If ((G.<=?) x y)
                                ('Pos ((G.-) y x))
                                ('Neg ((G.-) x y))
  (x :: Nat)  + 'Pos y      = 'Pos ((G.+) x y)
  (x :: Nat)  + 'Neg y      = If ((G.<=?) y x)
                                ('Pos ((G.-) x y))
                                ('Neg ((G.-) y x))

  (n1 ':% d1) + (n2 ':% d2) = ((n1 * d2) + (n2 * d1)) ':% (d1 * d2)
  (n ':% d)   + y           = ((d * y) + n) ':% d
  x           + (n ':% d)   = ((d * x) + n) ':% d

type family (x :: k1) - (y :: k2) :: ArithK k1 k2 where
  (x :: Nat)  - (y :: Nat)  = (G.-) x y

  'Pos x      - 'Pos y      = 'Pos x + 'Neg y
  'Neg x      - 'Pos y      = 'Neg x + 'Neg y
  'Pos x      - 'Neg y      = 'Pos x + 'Pos y
  'Neg x      - 'Neg y      = 'Neg x + 'Pos y

  'Pos x      - (y :: Nat)  = 'Pos x + 'Neg y
  'Neg x      - (y :: Nat)  = 'Neg x + 'Neg y
  (x :: Nat)  - 'Pos y      = 'Pos x + 'Neg y
  (x :: Nat)  - 'Neg y      = 'Pos x + 'Pos y

  -- Denominators are wrapped in Pos here to force the products to
  -- evaluate as kind TInt instead of Nat.  Without this, it is possible
  -- to end up with for example @(14 - 15) :: Nat@ which does not produce
  -- a Nat and therefore causes typing to fail.
  (n1 ':% d1) - (n2 ':% d2) = ((n1 * 'Pos d2) - (n2 * 'Pos d1)) ':% (d1 * d2)
  (n ':% d)   - y           = (n - ('Pos d * y)) ':% d
  x           - (n ':% d)   = (('Pos d * x) - n) ':% d


-- | The product of two type-level numbers
type family (x :: k1) * (y :: k2) :: ArithK k1 k2 where
  (x :: Nat)  * (y :: Nat)  = (G.*) x y

  'Pos x      * 'Pos y      = 'Pos ((G.*) x y)
  'Neg x      * 'Pos y      = 'Neg ((G.*) x y)
  'Pos x      * 'Neg y      = 'Neg ((G.*) x y)
  'Neg x      * 'Neg y      = 'Pos ((G.*) x y)

  'Neg x      * (y :: Nat)  = 'Neg ((G.*) x y)
  'Pos x      * (y :: Nat)  = 'Pos ((G.*) x y)
  (x :: Nat)  * 'Neg y      = 'Neg ((G.*) x y)
  (x :: Nat)  * 'Pos y      = 'Pos ((G.*) x y)

  (n1 ':% d1) * (n2 ':% d2) = (n1 * n2) ':% (d1 * d2)
  (n ':% d)   * y           = (n * y) ':% d
  x           * (n ':% d)   = (x * n) ':% d
