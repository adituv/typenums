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

-- | This module exposes the inner workings of type-level arithmetic for
--   further extensions.  The type families found within this module should
--   not be used directly other than to add new type instances; instead use
--   the operators from "Data.TypeNums.Arithmetic".
--
--   Due to technical limitations, the type families are open for extension
--   only in GHC 8.2 and onwards.
-- @since 0.1.2
module Data.TypeNums.Arithmetic.Internal where

import Data.Type.Bool     (If)
import Data.TypeNums.Ints
import Data.TypeNums.Rats
import GHC.TypeLits

-- TODO: Reduce type-level rationals?  Probably not 100% necessary as the
-- user should only see them via ratVal which already handles reduction
-- at value level.

#if MIN_VERSION_base(4,10,0)

-- NB. The cases for the result kind type families are exhaustively
-- enumerated so that they can be compatible with further extensions without
-- subsuming cases that extensions want to handle separately.  An extension
-- could validly write e.g. "type instance AddK a Inf" for its own local use,
-- but if any code attempts to use both that instance and another extension
-- it may well conflict.

-- | The kind of the result of addition.  If you add new cases to this
--   type family, be aware that it could cause conflicts with other code
--   that does the same.  Modify with caution.
type family AddK k1 k2
type instance AddK Nat  Nat  = Nat
type instance AddK Nat  Rat  = Rat
type instance AddK Nat  TInt = TInt
type instance AddK Rat  Nat  = Rat
type instance AddK Rat  Rat  = Rat
type instance AddK Rat  TInt = Rat
type instance AddK TInt Nat  = TInt
type instance AddK TInt Rat  = Rat
type instance AddK TInt TInt = TInt

-- | The kind of the result of subtraction.  If you add new cases to this
--   type family, be aware that it could cause conflicts with other code
--   that does the same.  Modify with caution.
type family SubK k1 k2
type instance SubK Nat  Nat  = Nat
type instance SubK Nat  Rat  = Rat
type instance SubK Nat  TInt = TInt
type instance SubK Rat  Nat  = Rat
type instance SubK Rat  Rat  = Rat
type instance SubK Rat  TInt = Rat
type instance SubK TInt Nat  = TInt
type instance SubK TInt Rat  = Rat
type instance SubK TInt TInt = TInt

-- | The kind of the result of multiplication.  If you add new cases to this
--   type family, be aware that it could cause conflicts with other code
--   that does the same.  Modify with caution.
type family MulK k1 k2
type instance MulK Nat  Nat  = Nat
type instance MulK Nat  Rat  = Rat
type instance MulK Nat  TInt = TInt
type instance MulK Rat  Nat  = Rat
type instance MulK Rat  Rat  = Rat
type instance MulK Rat  TInt = Rat
type instance MulK TInt Nat  = TInt
type instance MulK TInt Rat  = Rat
type instance MulK TInt TInt = TInt

#else

-- | The kind of the result of addition.  If you add new cases to this
--   type family, be aware that it could cause conflicts with other code
--   that does the same.  Modify with caution.
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

-- | The kind of the result of subtraction.  If you add new cases to this
--   type family, be aware that it could cause conflicts with other code
--   that does the same.  Modify with caution.
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

-- | The kind of the result of multiplication.  If you add new cases to this
--   type family, be aware that it could cause conflicts with other code
--   that does the same.  Modify with caution.
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

#endif


-- | The sum of two type-level numbers.
type family Add (x :: k1) (y :: k2) :: AddK k1 k2
type instance Add (x :: Nat) (y :: Nat)  = x + y

type instance Add ('Pos x)   ('Pos y)   = 'Pos (x + y)
type instance Add ('Neg x)   ('Pos y)   = If (x <=? y)
                                            ('Pos (y - x))
                                            ('Neg (x - y))
type instance Add ('Pos x)   ('Neg y)   = If (y <=? x)
                                            ('Pos (x - y))
                                            ('Neg (y - x))
type instance Add ('Neg x)   ('Neg y)   = 'Neg (x + y)

type instance Add ('Pos x)   (y :: Nat) = 'Pos (x + y)
type instance Add ('Neg x)   (y :: Nat) = If (x <=? y)
                                            ('Pos (y - x))
                                            ('Neg (x - y))
type instance Add (x :: Nat)  ('Pos y)  = 'Pos (x + y)
type instance Add (x :: Nat)  ('Neg y)  = If (y <=? x)
                                            ('Pos (x - y))
                                            ('Neg (y - x))

type instance Add (n1 ':% d1) (n2 ':% d2) = (Add (Mul n1 d2) (Mul n2 d1))
                                            ':% (Mul d1 d2)
type instance Add (n ':% d)   (y :: Nat)  = (Add n (Mul d y)) ':% d
type instance Add (n ':% d)   ('Pos y)    = (Add n (Mul d y)) ':% d
type instance Add (n ':% d)   ('Neg y)    = (Add n (Mul d ('Neg y))) ':% d
type instance Add (x :: Nat)  (n ':% d)   = (Add (Mul d x) n) ':% d
type instance Add ('Pos x)    (n ':% d)   = (Add (Mul d x) n) ':% d
type instance Add ('Neg x)    (n ':% d)   = (Add (Mul d ('Neg x)) n) ':% d

-- | The difference of two type-level numbers
type family Sub (x :: k1) (y :: k2) :: SubK k1 k2
type instance Sub (x :: Nat) (y :: Nat)  = x - y

type instance Sub ('Pos x)   ('Pos y)   = Add x ('Neg y)
type instance Sub ('Neg x)   ('Pos y)   = Add ('Neg x) ('Neg y)
type instance Sub ('Pos x)   ('Neg y)   = Add ('Pos x) ('Pos y)
type instance Sub ('Neg x)   ('Neg y)   = Add ('Neg x) ('Pos y)

type instance Sub ('Pos x)   (y :: Nat) = Add ('Pos x) ('Neg y)
type instance Sub ('Neg x)   (y :: Nat) = Add ('Neg x) ('Neg y)
type instance Sub (x :: Nat)  ('Pos y)  = Add x ('Neg y)
type instance Sub (x :: Nat)  ('Neg y)  = Add x ('Pos y)

-- Denominators are wrapped in Pos here to force the products to evaluate
-- as kind TInt instead of Nat.  Without this, it is possible to end up with,
-- for example, @(14-15) :: Nat@ which does not produce a Nat and therefore
-- causes typing to fail.
type instance Sub (n1 ':% d1) (n2 ':% d2) = (Sub (Mul n1 ('Pos d2)) (Mul n2 ('Pos d1))) ':% (Mul d1 d2)
type instance Sub (n ':% d)   (y :: Nat)  = Sub (n ':% d) (y ':% 1)
type instance Sub (n ':% d)   ('Pos y)    = Sub (n ':% d) ('Pos y ':% 1)
type instance Sub (n ':% d)   ('Neg y)    = Sub (n ':% d) ('Neg y ':% 1)
type instance Sub (x :: Nat)  (n ':% d)   = Sub (x ':% 1) (n ':% d)
type instance Sub ('Pos x)    (n ':% d)   = Sub ('Pos x ':% 1) (n ':% d)
type instance Sub ('Neg x)    (n ':% d)   = Sub ('Neg x ':% 1) (n ':% d)

-- | The product of two type-level numbers
type family Mul (x :: k1) (y :: k2) :: MulK k1 k2
type instance Mul (x :: Nat) (y :: Nat) = x * y

type instance Mul ('Pos x)   ('Pos y)   = 'Pos (x * y)
type instance Mul ('Neg x)   ('Pos y)   = 'Neg (x * y)
type instance Mul ('Pos x)   ('Neg y)   = 'Neg (x * y)
type instance Mul ('Neg x)   ('Neg y)   = 'Pos (x * y)

type instance Mul ('Pos x)   (y :: Nat) = 'Pos (x * y)
type instance Mul ('Neg x)   (y :: Nat) = 'Neg (x * y)
type instance Mul (x :: Nat)  ('Pos y)  = 'Pos (x * y)
type instance Mul (x :: Nat)  ('Neg y)  = 'Neg (x * y)

type instance Mul (n1 ':% d1) (n2 ':% d2) = (Mul n1 n2) ':% (Mul d1 d2)
type instance Mul (n ':% d)   (y :: Nat)  = (Mul n y) ':% d
type instance Mul (n ':% d)   ('Pos y)    = (Mul n ('Pos y)) ':% d
type instance Mul (n ':% d)   ('Neg y)    = (Mul n ('Neg y)) ':% d
type instance Mul (x :: Nat)  (n ':% d)   = (Mul x n) ':% d
type instance Mul ('Pos x)    (n ':% d)   = (Mul ('Pos x) n) ':% d
type instance Mul ('Neg x)    (n ':% d)   = (Mul ('Neg x) n) ':% d
