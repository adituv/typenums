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
  -- * Kind Results
    AddK
  , SubK
  , MulK
  , IntDivK
  , ExpK

  -- * Unary operations
  , Abs
  , Negate
  , Recip
  , Simplify

  -- ** Rounding operations
  , Truncate
  , Floor
  , Ceiling

  -- * Binary operations
  , Add
  , Sub
  , Mul
  , RatDiv
  , DivMod
  , QuotRem
  , Div
  , Mod
  , Quot
  , Rem
  , GCD
  , Exp
) where

import Data.Type.Bool     (If)
import Data.TypeNums.Ints
import Data.TypeNums.Rats.Type
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
  MulK k    k    = k
  MulK Nat  Rat  = Rat
  MulK Nat  TInt = TInt
  MulK Rat  Nat  = Rat
  MulK Rat  TInt = Rat
  MulK TInt Nat  = TInt
  MulK TInt Rat  = Rat

-- | The kind of the result of division by a natural number
--
-- @since 0.1.4
type family IntDivK k where
  IntDivK Nat = Nat
  IntDivK TInt = TInt

-- | The kind of the result of type-level exponentiation
type family ExpK k1 k2 where
  ExpK Nat Nat = Nat
  ExpK TInt Nat = TInt
  ExpK Rat Nat = Rat
  ExpK _   TInt = Rat

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
type family RatDiv (x :: k1) (y :: k2) :: Rat where
  RatDiv (x :: Nat) (y :: Nat) = x ':% y
  RatDiv (x :: TInt) (y :: Nat) = x ':% y
  RatDiv (x :: Nat) ('Pos y) = ('Pos x) ':% y
  RatDiv (x :: Nat) ('Neg y) = ('Neg x) ':% y
  RatDiv (x :: TInt) ('Neg y) = (Negate x) ':% y
  RatDiv (x :: TInt) ('Pos y) = x ':% y
  RatDiv (x :: Rat) (y :: Rat) = Mul x (Recip y)
  RatDiv x (y :: Rat) = Mul (RatDiv x 1) (Recip y)
  RatDiv (x :: Rat) y = Mul x (RatDiv 1 y)

-- | The result of negating a 'TInt'
--
-- @since 0.1.4
type family Negate (x :: TInt) :: TInt where
  Negate ('Pos 0) = 'Pos 0
  Negate ('Neg 0) = 'Pos 0
  Negate ('Pos x) = 'Neg x
  Negate ('Neg x) = 'Pos x

-- | The quotient and remainder of a type-level integer and a natural number.
--   For a negative dividend, the remainder part is positive such that
--   x = q*y + r
-- @since 0.1.4
type family DivMod (x :: k) (y :: Nat) :: (IntDivK k, IntDivK k) where
  DivMod _ 0 = G.TypeError ('G.Text "Divisor must not be 0")
  DivMod (x :: Nat) y = UnPos (DivModAux ('Pos x) y ('Pos 0))
  DivMod ('Pos x) y = DivModAux ('Pos x) y ('Pos 0)
  DivMod ('Neg x) y = DivModNegFixup (DivModAux ('Pos x) y ('Pos 0)) y

-- | The quotient and remainder of a type-level integer and a natural number.
--   For a negative dividend, the remainder part is negative such that
--   x = q*y + r
-- @since 0.1.4
type family QuotRem (x :: k) (y :: Nat) :: (IntDivK k, IntDivK k) where
  QuotRem ('Neg x) y = QuotRemFixup (DivMod ('Neg x) y) y
  QuotRem x y = DivMod x y

-- | The quotient of a type-level integer and a natural number.
--
-- @since 0.1.4
type family Div (x :: k) (y :: Nat) :: IntDivK k where
  Div x y = Fst (DivMod x y)

-- | The remainder of a type-level integer and a natural number
--   For a negative number, behaves similarly to 'mod'.
-- @since 0.1.4
type family Mod (x :: k) (y :: Nat) :: IntDivK k where
  Mod x y = Snd (DivMod x y)

-- Fixes up the calculated quotient for a negative dividend.
-- Subtracts 1 to obtain the behaviour of 'div' instead of 'quot'
type family DivModNegFixup (x :: (TInt, TInt)) (y :: Nat) :: (TInt, TInt) where
  DivModNegFixup '(a, 'Pos 0) y = '( Negate a, 'Pos 0 )
  DivModNegFixup '(a, 'Neg 0) y = '( Negate a, 'Pos 0 )
  DivModNegFixup '(a, b) y = '( Negate (Add 1 a), Sub y b )

-- Converts the result of DivMod for a negative dividend
-- into the result of QuotRem for a negative dividend
type family QuotRemFixup (x :: (TInt, TInt)) (y :: Nat) :: (TInt, TInt) where
  QuotRemFixup '(d, 'Pos 0) y = '( d, 'Pos 0 )
  QuotRemFixup '(d, 'Neg 0) y = '( d, 'Pos 0 )
  QuotRemFixup '(d,m) y = '(Add 1 d, Sub m y)

-- | The integer part of the result of dividing an integer by a
--   natural number
--
-- @since 0.1.4
type family Quot (x :: k) (y :: Nat) :: IntDivK k where
  Quot x y = Fst (QuotRem x y)

-- | The remainder of the result of dividing an integer by a
--   natural number
--
-- @since 0.1.4
type family Rem (x :: k) (y :: Nat) :: IntDivK k where
  Rem x y = Snd (QuotRem x y)

-- Integer division of positive / positive
type family DivModAux (x :: TInt) (y :: Nat) (a :: TInt) :: (TInt, TInt) where
  DivModAux ('Pos x) y a = DivModAux (Sub ('Pos x) y) y (Add 1 a)
  DivModAux ('Neg x) y a = '(Sub a 1, Sub y ('Pos x))

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
type family GCD (x :: k1) (y :: k2) :: Nat where
  GCD (x :: Nat) (y :: Nat) = GCDAux x y
  GCD (x :: Nat) (y :: TInt) = GCDAux x (UnPos (Abs y))
  GCD (x :: TInt) (y :: Nat) = GCDAux (UnPos (Abs x)) y
  GCD (x :: TInt) (y :: TInt) = GCDAux (UnPos (Abs x)) (UnPos (Abs y))

-- Euclidean algorithm for calculating the GCD of two natural numbers
type family GCDAux (x :: Nat) (y :: Nat) :: Nat where
  GCDAux x 0 = x
  GCDAux x y = GCDAux y (Rem x y)

-- | Reduce a type-level rational into its canonical form
--
-- @since 0.1.4
type family Simplify (x :: Rat) :: Rat where
  Simplify ((x :: Nat) ':% y) = (Quot x (GCD x y)) ':% (Quot y (GCD x y))
  Simplify ((x :: TInt) ':% y) = (Quot x (GCD x y)) ':% (Quot y (GCD x y))

-- | Exponentiation of a type-level number by an integer
--
-- @since 0.1.4
type family Exp (x :: k1) (y :: k2) :: ExpK k1 k2 where
  Exp (x :: Nat)  (y :: Nat) = ExpAux 1 x y
  Exp (x :: TInt) (y :: Nat) = ExpAux ('Pos 1) x y
  Exp (x :: Rat)  (y :: Nat) = ExpAux (1 ':% 1) x y
  Exp (x :: Rat)  ('Pos y)   = ExpAux (1 ':% 1) x y
  Exp (x :: Rat)  ('Neg y)   = Recip (ExpAux (1 ':% 1) x y)
  Exp x           ('Pos y)   = ExpAux (1 ':% 1) (x ':% 1) y
  Exp x           ('Neg y)   = Recip (ExpAux (1 ':% 1) (x ':% 1) y)

type family ExpAux (acc :: k1) (x :: k1) (y :: Nat) :: k1 where
  ExpAux acc _ 0 = acc
  ExpAux acc x y = ExpAux (Mul x acc) x (Sub y 1)

-- | Round a type-level number towards zero
--
-- @since 0.1.4
type family Truncate (x :: k) :: TInt where
  Truncate (x :: Nat)          = 'Pos x
  Truncate (x :: TInt)         = x
  Truncate (x ':% 0)           = G.TypeError ('G.Text ("The denominator must not be 0"))
  Truncate ((x :: Nat) ':% y)  = 'Pos (Quot x y)
  Truncate ((x :: TInt) ':% y) = Quot x y

-- | Round a type-level number towards negative infinity
--
-- @since 0.1.4
type family Floor (x :: k) :: TInt where
  Floor (x :: Nat)          = 'Pos x
  Floor (x :: TInt)         = x
  Floor (x ':% 0)           = G.TypeError ('G.Text ("The denominator must not be 0"))
  Floor ((x :: Nat) ':% y)  = 'Pos (Div x y)
  Floor ((x :: TInt) ':% y) = Div x y

-- | Round a type-level number towards positive infinity
--
-- @since 0.1.4
type family Ceiling (x :: k) :: TInt where
  Ceiling (x :: Nat)  = 'Pos x
  Ceiling (x :: TInt) = x
  Ceiling (x ':% 0)   = G.TypeError ('G.Text ("The denominator must not be 0"))
  Ceiling (x ':% y)   = Add 1 (Floor (Sub x 1 ':% y))
