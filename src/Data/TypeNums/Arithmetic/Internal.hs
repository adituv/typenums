{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType #-}
#endif

-- | This module exposes the inner workings of type-level arithmetic for
--   further extensions.
module Data.TypeNums.Arithmetic.Internal (
  AddK, SubK, MulK, IntDivK, Abs, Negate, Recip, Simplify, Add, Sub, Mul, Div, DivMod, QuotRem, IntDiv, Mod, Quot, Rem, GCD
) where

import Data.Type.Bool     (If)
import Data.TypeNums.Equality (type (==?))
import Data.TypeNums.Ints
import Data.TypeNums.Rats
import GHC.TypeLits(Nat)
import qualified GHC.TypeLits as G

-- TODO: Reduce type-level rationals?  Probably not 100% necessary as the
-- user should only see them via ratVal which already handles reduction
-- at value level.

-- | The kind of the result of addition.
--
-- @since 0.1.2
type family AddK k1 k2 where
  AddK Nat  Nat  = Nat
  AddK Nat  Rat  = Rat
  AddK Nat  TInt = TInt
  AddK Rat  Nat  = Rat
  AddK Rat  Rat  = Rat
  AddK Rat  TInt = Rat
  AddK TInt Nat  = TInt
  AddK TInt Rat  = Rat
  AddK TInt TInt = TInt

-- | The kind of the result of subtraction.
--
-- @since 0.1.2
type family SubK k1 k2 where
  SubK Nat  Nat  = Nat
  SubK Nat  Rat  = Rat
  SubK Nat  TInt = TInt
  SubK Rat  Nat  = Rat
  SubK Rat  Rat  = Rat
  SubK Rat  TInt = Rat
  SubK TInt Nat  = TInt
  SubK TInt Rat  = Rat
  SubK TInt TInt = TInt

-- | The kind of the result of multiplication.
--
-- @since 0.1.2
type family MulK k1 k2 where
  MulK Nat  Nat  = Nat
  MulK Nat  Rat  = Rat
  MulK Nat  TInt = TInt
  MulK Rat  Nat  = Rat
  MulK Rat  Rat  = Rat
  MulK Rat  TInt = Rat
  MulK TInt Nat  = TInt
  MulK TInt Rat  = Rat
  MulK TInt TInt = TInt

-- | The kind of the result of division by a natural number
--
-- @since 0.1.4
type family IntDivK k where
  IntDivK Nat = Nat
  IntDivK TInt = TInt

-- | The kind of the result of finding the GCD of two integers
--
-- @since 0.1.4
type family GCDK k1 k2 where
  GCDK Nat Nat = Nat
  GCDK TInt _ = TInt
  GCDK _ TInt = TInt

-- | The sum of two type-level numbers.
--
-- @since 0.1.2
type family Add (x :: k1) (y :: k2) :: AddK k1 k2 where
  Add (x :: Nat) (y :: Nat)  = x G.+ y

  Add ('Pos x)   ('Pos y)   = 'Pos (x G.+ y)
  Add ('Neg x)   ('Pos y)   = If (x G.<=? y)
                                            ('Pos (y G.- x))
                                            ('Neg (x G.- y))
  Add ('Pos x)   ('Neg y)   = If (y G.<=? x)
                                            ('Pos (x G.- y))
                                            ('Neg (y G.- x))
  Add ('Neg x)   ('Neg y)   = 'Neg (x G.+ y)

  Add ('Pos x)   (y :: Nat) = 'Pos (x G.+ y)
  Add ('Neg x)   (y :: Nat) = If (x G.<=? y)
                                            ('Pos (y G.- x))
                                            ('Neg (x G.- y))
  Add (x :: Nat)  ('Pos y)  = 'Pos (x G.+ y)
  Add (x :: Nat)  ('Neg y)  = If (y G.<=? x)
                                            ('Pos (x G.- y))
                                            ('Neg (y G.- x))

  Add (n1 ':% d1) (n2 ':% d2) = (Add (Mul n1 d2) (Mul n2 d1))
                                            ':% (Mul d1 d2)
  Add (n ':% d)   (y :: Nat)  = (Add n (Mul d y)) ':% d
  Add (n ':% d)   ('Pos y)    = (Add n (Mul d y)) ':% d
  Add (n ':% d)   ('Neg y)    = (Add n (Mul d ('Neg y))) ':% d
  Add (x :: Nat)  (n ':% d)   = (Add (Mul d x) n) ':% d
  Add ('Pos x)    (n ':% d)   = (Add (Mul d x) n) ':% d
  Add ('Neg x)    (n ':% d)   = (Add (Mul d ('Neg x)) n) ':% d

-- | The difference of two type-level numbers
--
-- @since 0.1.2
type family Sub (x :: k1) (y :: k2) :: SubK k1 k2 where
  Sub (x :: Nat) (y :: Nat)  = x G.- y

  Sub ('Pos x)   ('Pos y)   = Add x ('Neg y)
  Sub ('Neg x)   ('Pos y)   = Add ('Neg x) ('Neg y)
  Sub ('Pos x)   ('Neg y)   = Add ('Pos x) ('Pos y)
  Sub ('Neg x)   ('Neg y)   = Add ('Neg x) ('Pos y)

  Sub ('Pos x)   (y :: Nat) = Add ('Pos x) ('Neg y)
  Sub ('Neg x)   (y :: Nat) = Add ('Neg x) ('Neg y)
  Sub (x :: Nat)  ('Pos y)  = Add x ('Neg y)
  Sub (x :: Nat)  ('Neg y)  = Add x ('Pos y)

-- Denominators are wrapped in Pos here to force the products to evaluate
-- as kind TInt instead of Nat.  Without this, it is possible to end up with,
-- for example, @(14-15) :: Nat@ which does not produce a Nat and therefore
-- causes typing to fail.
  Sub (n1 ':% d1) (n2 ':% d2) = (Sub (Mul n1 ('Pos d2)) (Mul n2 ('Pos d1))) ':% (Mul d1 d2)
  Sub (n ':% d)   (y :: Nat)  = Sub (n ':% d) (y ':% 1)
  Sub (n ':% d)   ('Pos y)    = Sub (n ':% d) ('Pos y ':% 1)
  Sub (n ':% d)   ('Neg y)    = Sub (n ':% d) ('Neg y ':% 1)
  Sub (x :: Nat)  (n ':% d)   = Sub (x ':% 1) (n ':% d)
  Sub ('Pos x)    (n ':% d)   = Sub ('Pos x ':% 1) (n ':% d)
  Sub ('Neg x)    (n ':% d)   = Sub ('Neg x ':% 1) (n ':% d)

-- | The product of two type-level numbers
--
-- @since 0.1.2
type family Mul (x :: k1) (y :: k2) :: MulK k1 k2 where
  Mul (x :: Nat) (y :: Nat) = x G.* y

  Mul ('Pos x)   ('Pos y)   = 'Pos (x G.* y)
  Mul ('Neg x)   ('Pos y)   = 'Neg (x G.* y)
  Mul ('Pos x)   ('Neg y)   = 'Neg (x G.* y)
  Mul ('Neg x)   ('Neg y)   = 'Pos (x G.* y)

  Mul ('Pos x)   (y :: Nat) = 'Pos (x G.* y)
  Mul ('Neg x)   (y :: Nat) = 'Neg (x G.* y)
  Mul (x :: Nat)  ('Pos y)  = 'Pos (x G.* y)
  Mul (x :: Nat)  ('Neg y)  = 'Neg (x G.* y)

  Mul (n1 ':% d1) (n2 ':% d2) = (Mul n1 n2) ':% (Mul d1 d2)
  Mul (n ':% d)   (y :: Nat)  = (Mul n y) ':% d
  Mul (n ':% d)   ('Pos y)    = (Mul n ('Pos y)) ':% d
  Mul (n ':% d)   ('Neg y)    = (Mul n ('Neg y)) ':% d
  Mul (x :: Nat)  (n ':% d)   = (Mul x n) ':% d
  Mul ('Pos x)    (n ':% d)   = (Mul ('Pos x) n) ':% d
  Mul ('Neg x)    (n ':% d)   = (Mul ('Neg x) n) ':% d

-- | The reciprocal of a type-level rational
--
-- @since 0.1.4
type family Recip (x :: Rat) :: Rat where
  Recip (('Pos x) ':% y) = ('Pos y) ':% x
  Recip (('Neg x) ':% y) = ('Neg y) ':% x
  Recip ((x :: Nat) ':% y) = y ':% x

-- | The result of dividing two type-level numbers.
--
-- @since 0.1.4
type family Div (x :: k1) (y :: k2) :: Rat where
  Div (x :: Nat) (y :: Nat) = x ':% y
  Div (x :: TInt) (y :: Nat) = x ':% y
  Div (x :: Nat) ('Neg y) = ('Neg x) ':% y
  Div (x :: TInt) ('Neg y) = (Negate x) ':% y
  Div (x :: TInt) ('Pos y) = x ':% y
  Div (x :: Rat) (y :: Rat) = Mul x (Recip y)
  Div x (y :: Rat) = Mul (Div x 1) (Recip y)
  Div (x :: Rat) y = Mul (Recip x) (Div y 1)

-- | The result of negating a 'TInt'
-- 
-- @since 0.1.4
type family Negate (x :: TInt) :: TInt where
  Negate ('Pos 0) = 'Pos 0
  Negate ('Neg 0) = 'Pos 0
  Negate ('Pos x) = 'Neg x
  Negate ('Neg x) = 'Pos x

-- | The quotient and remainder of a type-level integer and a natural number.
-- 
-- @since 0.1.4
type family DivMod (x :: k) (y :: Nat) :: (IntDivK k, IntDivK k) where
  DivMod _ 0 = G.TypeError ('G.Text "Divisor must not be 0")
  DivMod (x :: Nat) y = UnPos (DivModAux ('Pos x) y ('Pos 0))
  DivMod ('Pos x) y = DivModAux ('Pos x) y ('Pos 0)
  DivMod ('Neg x) y = DivModNegFixup (DivModAux ('Pos x) y ('Pos 0))

-- |
--
-- @since 0.1.4
type family QuotRem (x :: k) (y :: Nat) :: (IntDivK k, IntDivK k) where
  QuotRem ('Neg x) y = QuotRemFixup (DivMod ('Neg x) y) y
  QuotRem x y = DivMod x y

-- | The quotient of a type-level integer and a natural number.
--   
-- @since 0.1.4
type family IntDiv (x :: k) (y :: Nat) :: IntDivK k where
  IntDiv x y = Fst (DivMod x y)

-- | The remainder of a type-level integer and a natural number
--   For a negative number, behaves similarly to 'mod'.
-- @since 0.1.4
type family Mod (x :: k) (y :: Nat) :: IntDivK k where
  Mod x y = Snd (DivMod x y)

-- Fixes up the calculated quotient for a negative dividend.
-- Subtracts 1 to obtain the behaviour of 'div' instead of 'quot'
type family DivModNegFixup (x :: (TInt, k)) :: (TInt, k) where
  DivModNegFixup '(a, b) = '( Negate (Add 1 a), b )

-- Converts the result of DivMod for a negative dividend
-- into the result of QuotRem for a negative dividend
type family QuotRemFixup (x :: (TInt, TInt)) (y :: Nat) :: (TInt, TInt) where
  QuotRemFixup '(d,m) y = '(Add 1 d, Sub m y)

-- |
--
-- @since 0.1.4
type family Quot (x :: k) (y :: Nat) :: IntDivK k where
  Quot x y = Fst (QuotRem x y)

-- |
--
-- @since 0.1.4
type family Rem (x :: k) (y :: Nat) :: IntDivK k where
  Rem x y = Snd (QuotRem x y)

-- Integer division of positive / positive
type family DivModAux (x :: TInt) (y :: Nat) (a :: TInt) :: (TInt, TInt) where
  DivModAux ('Pos x) y a = DivModAux (Sub ('Pos x) y) y (Add 1 a)
  DivModAux ('Neg x) y a = '(Sub a 1, If (x ==? y) ('Pos 0) ('Pos x))

-- Internal function to unwrap a Pos TInt to a Nat
type family UnPos (x :: k1) :: k2 where
  UnPos ('Pos x) = x
  UnPos '( 'Pos x, 'Pos y) = '(x, y)

type family Fst (x :: (k1, k2)) :: k1 where
  Fst '(x,y) = x

type family Snd (x :: (k1, k2)) :: k2 where
  Snd '(x,y) = y

-- | The absolute value of a type-level number
--
-- @since 0.1.4
type family Abs (x :: k) :: k where
  Abs (x :: Nat) = x
  Abs ('Pos x) = 'Pos x
  Abs ('Neg x) = 'Pos x
  Abs (x ':% y) = Abs x ':% y

-- | The greatest common divisor of two type-level integers
--
-- @since 0.1.4
type family GCD (x :: k1) (y :: k2) :: GCDK k1 k2 where
  GCD (x :: Nat) (y :: Nat) = GCDAux x y
  GCD (x :: Nat) (y :: TInt) = 'Pos (GCDAux x (UnPos (Abs y)))
  GCD (x :: TInt) (y :: Nat) = 'Pos (GCDAux (UnPos (Abs x)) y)

-- Euclidean algorithm for calculating the GCD of two natural numbers
type family GCDAux (x :: Nat) (y :: Nat) :: Nat where
  GCDAux x 0 = x
  GCDAux x y = GCDAux y (Rem x y)

-- | Reduce a type-level rational into its canonical form
--
-- @since 0.1.4
type family Simplify (x :: Rat) :: Rat where
  Simplify ((x :: Nat) ':% y) = (IntDiv x (GCD x y)) ':% (IntDiv y (GCD x y))
  Simplify ((x :: TInt) ':% y) = (IntDiv x (UnPos (GCD x y))) ':% (IntDiv y (UnPos (GCD x y)))
