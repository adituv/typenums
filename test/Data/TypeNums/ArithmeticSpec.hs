{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType #-}
#endif

module Data.TypeNums.ArithmeticSpec where

import Data.TypeNums
import Test.Hspec

import Data.Proxy
import Data.Ratio
import GHC.Exts(Proxy#, proxy#)

spec :: Spec
spec = do
  additionTests
  subtractionTests
  multiplicationTests
  absTests
  negateTests
  recipTests
  divisionTests
  divModTests
  quotRemTests
  gcdTests
  simplifyTests

additionTests :: Spec
additionTests =
  describe "addition" $ do
    it "correctly adds two natural numbers" $
      ratVal (Proxy @(3 + 5)) `shouldBe` 8
    it "correctly adds a negative integer and a natural" $
      ratVal (Proxy @((Neg 4) + 3)) `shouldBe` (-1)
    it "correctly adds a natural and a negative integer" $
      ratVal (Proxy @(8 + (Neg 7))) `shouldBe` 1
    it "correctly adds two negative integers" $
      ratVal (Proxy @((Neg 5) + (Neg 6))) `shouldBe` (-11)
    it "correctly adds two positive integers" $
      ratVal (Proxy @((Pos 5) + (Pos 6))) `shouldBe` 11
    it "correctly adds a negative integer and a positive integer" $
      ratVal (Proxy @((Neg 3) + (Pos 5))) `shouldBe` 2
    it "correctly adds a positive integer and a negative integer" $
      ratVal (Proxy @((Pos 7) + (Neg 8))) `shouldBe` (-1)
    it "correctly adds two rationals" $
      ratVal (Proxy @((2 ':% 3) + (5 ':% 7))) `shouldBe` (29 % 21)
    it "correctly adds a rational with a natural" $
      ratVal (Proxy @((2 ':% 3) + 2)) `shouldBe` (8 % 3)
    it "correctly adds a natural with a rational" $
      ratVal (Proxy @(2 + (2 ':% 3))) `shouldBe` (8 % 3)
    it "correctly adds a rational with a negative integer" $
      ratVal (Proxy @((2 ':% 3) + (Neg 2))) `shouldBe` ((-4) % 3)
    it "correctly adds a negative integer with a rational" $
      ratVal (Proxy @((Neg 2) + (2 ':% 3))) `shouldBe` ((-4) % 3)
    it "correctly adds a rational with a positive integer" $
      ratVal (Proxy @((2 ':% 3) + (Pos 2))) `shouldBe` 8 % 3
    it "correctly adds a positive integer with a rational" $
      ratVal (Proxy @((Pos 2) + (2 ':% 3))) `shouldBe` 8 % 3

subtractionTests :: Spec
subtractionTests =
  describe "subtraction" $ do
    it "correctly subtracts two natural numbers" $
      ratVal (Proxy @(5 - 3)) `shouldBe` 2
    it "correctly subtracts a negative integer and a natural" $
      ratVal (Proxy @((Neg 4) - 3)) `shouldBe` (-7)
    it "correctly subtracts a natural and a negative integer" $
      ratVal (Proxy @(8 - (Neg 7))) `shouldBe` 15
    it "correctly subtracts two negative integers" $
      ratVal (Proxy @((Neg 5) - (Neg 6))) `shouldBe` 1
    it "correctly subtracts two positive integers" $
      ratVal (Proxy @((Pos 5) - (Pos 6))) `shouldBe` (-1)
    it "correctly subtracts a negative integer and a positive integer" $
      ratVal (Proxy @((Neg 3) - (Pos 5))) `shouldBe` (-8)
    it "correctly subtracts a positive integer and a negative integer" $
      ratVal (Proxy @((Pos 7) - (Neg 8))) `shouldBe` 15
    it "correctly subtracts two rationals" $
      ratVal (Proxy @((2 ':% 3) - (5 ':% 7))) `shouldBe` ((-1) % 21)
    it "correctly subtracts a rational with a natural" $
      ratVal (Proxy @((2 ':% 3) - 2)) `shouldBe` ((-4) % 3)
    it "correctly subtracts a natural with a rational" $
      ratVal (Proxy @(2 - (2 ':% 3))) `shouldBe` (4 % 3)
    it "correctly subtracts a rational with a negative integer" $
      ratVal (Proxy @((2 ':% 3) - (Neg 2))) `shouldBe` (8 % 3)
    it "correctly subtracts a negative integer with a rational" $
      ratVal (Proxy @((Neg 2) - (2 ':% 3))) `shouldBe` ((-8) % 3)
    it "correctly subtracts a rational with a positive integer" $
      ratVal (Proxy @((2 ':% 3) - (Pos 2))) `shouldBe` (-4) % 3
    it "correctly subtracts a positive integer with a rational" $
      ratVal (Proxy @((Pos 2) - (2 ':% 3))) `shouldBe` 4 % 3

multiplicationTests :: Spec
multiplicationTests =
  describe "multiplication" $ do
    it "correctly multiplies two natural numbers" $
      ratVal (Proxy @(3 * 5)) `shouldBe` 15
    it "correctly multiplies a negative integer and a natural" $
      ratVal (Proxy @((Neg 4) * 3)) `shouldBe` (-12)
    it "correctly multiplies a natural and a negative integer" $
      ratVal (Proxy @(7 * (Neg 8))) `shouldBe` (-56)
    it "correctly multiplies two negative integers" $
      ratVal (Proxy @((Neg 5) * (Neg 6))) `shouldBe` 30
    it "correctly multiplies two positive integers" $
      ratVal (Proxy @((Pos 5) * (Pos 6))) `shouldBe` 30
    it "correctly multiplies a negative integer and a positive integer" $
      ratVal (Proxy @((Neg 4) * (Pos 3))) `shouldBe` (-12)
    it "correctly multiplies a positive integer and a negative integer" $
      ratVal (Proxy @((Pos 7) * (Neg 8))) `shouldBe` (-56)
    it "correctly multiplies two rationals" $
      ratVal (Proxy @((2 ':% 3) * (5 ':% 7))) `shouldBe` (10 % 21)
    it "correctly multiplies a rational with a natural" $
      ratVal (Proxy @((2 ':% 3) * 2)) `shouldBe` (4 % 3)
    it "correctly multiplies a natural with a rational" $
      ratVal (Proxy @(2 * (2 ':% 3))) `shouldBe` (4 % 3)
    it "correctly multiplies a rational with a negative integer" $
      ratVal (Proxy @((2 ':% 3) * (Neg 2))) `shouldBe` ((-4) % 3)
    it "correctly multiplies a negative integer with a rational" $
      ratVal (Proxy @((Neg 2) * (2 ':% 3))) `shouldBe` ((-4) % 3)
    it "correctly multiplies a rational with a positive integer" $
      ratVal (Proxy @((2 ':% 3) * (Pos 2))) `shouldBe` 4 % 3
    it "correctly multiplies a positive integer with a rational" $
      ratVal (Proxy @((Pos 2) * (2 ':% 3))) `shouldBe` 4 % 3

absTests :: Spec
absTests = do
  describe "abs" $ do
    it "Acts as a no-op on a natural" $
      natVal (Proxy @(Abs 3)) `shouldBe` 3
    it "Acts as a no-op on a positive int" $
      intVal (Proxy @(Abs (Pos 3))) `shouldBe` 3
    it "Negates a negative int" $
      intVal (Proxy @(Abs (Neg 3))) `shouldBe` 3

negateTests :: Spec
negateTests = do
  describe "negate" $ do
    it "Turns Pos into Neg" $
      intVal (Proxy @(Negate (Pos 3))) `shouldBe` (-3)
    it "Turns Neg into Pos" $
      intVal (Proxy @(Negate (Neg 3))) `shouldBe` 3

recipTests :: Spec
recipTests = do
  describe "recip" $ do
    it "Inverts Nat / Nat" $
      ratVal (Proxy @(Recip (3 :% 2))) `shouldBe` (2 % 3)
    it "Inverts Pos / Nat" $
      ratVal (Proxy @(Recip ((Pos 3) :% 2))) `shouldBe` (2 % 3)
    it "Inverts Neg / Nat" $
      ratVal (Proxy @(Recip ((Neg 3) :% 2))) `shouldBe` ((-2) % 3)

divisionTests :: Spec
divisionTests = do
  describe "ratdiv" $ do
    it "Divides two naturals" $
      ratVal (Proxy @(3 / 2)) `shouldBe` (3 % 2)
    it "Divides a positive integer by a natural" $
      ratVal (Proxy @((Pos 3) / 2)) `shouldBe` (3 % 2)
    it "Divides a negative integer by a natural" $
      ratVal (Proxy @((Neg 3) / 2)) `shouldBe` ((-3) % 2)
    it "Divides a natural by a positive integer" $
      ratVal (Proxy @(3 / (Pos 2))) `shouldBe` (3 % 2)
    it "Divides a natural by a negative integer" $
      ratVal (Proxy @(3 / (Neg 2))) `shouldBe` ((-3) % 2)
    it "Divides a positive integer by a negative integer" $
      ratVal (Proxy @((Pos 3) / (Neg 2))) `shouldBe` ((-3) % 2)
    it "Divides a negative integer by a positive integer" $
      ratVal (Proxy @((Neg 3) / (Pos 2))) `shouldBe` ((-3) % 2)
    it "Divides a negative integer by a negative integer" $
      ratVal (Proxy @((Neg 3) / (Neg 2))) `shouldBe` (3 % 2)
    it "Divides a natural by a rational" $
      ratVal (Proxy @(5 / (2 :% 3))) `shouldBe` (15 % 2)
    it "Divides a positive integer by a rational" $
      ratVal (Proxy @((Pos 5) / (2 :% 3))) `shouldBe` (15 % 2)
    it "Divides a negative integer by a rational" $
      ratVal (Proxy @((Neg 5) / (2 :% 3))) `shouldBe` ((-15) % 2)
    it "Divides a negative integer by a negative rational" $
      ratVal (Proxy @((Neg 5) / ((Neg 2) :% 3))) `shouldBe` (15 % 2)
    it "Divides a rational by a natural" $
      ratVal (Proxy @((2 :% 3) / 5)) `shouldBe` (2 % 15)
    it "Divides a rational by a positive integer" $
      ratVal (Proxy @((2 :% 3) / (Pos 5))) `shouldBe` (2 % 15)
    it "Divides a rational by a negative integer" $
      ratVal (Proxy @((2 :% 3) / (Neg 5))) `shouldBe` ((-2) % 15)
    it "Divides a negative rational by a negative integer" $
      ratVal (Proxy @(((Neg 2) :% 3) / (Neg 5))) `shouldBe` (2 % 15)
    it "Divides a rational by a rational" $
      ratVal (Proxy @((2 :% 3) / (5 :% 7))) `shouldBe` (14 % 15)

class IntPair (p :: (k1, k2)) where
  getIntVals :: Proxy p -> (Integer, Integer)

instance forall a b. (KnownInt a, KnownInt b) => IntPair '(a, b) where
  getIntVals _ = (intVal (Proxy @a), intVal (Proxy @b))

divModTests :: Spec
divModTests = do
  describe "divMod" $ do
    it "calculates divMod for two naturals" $
      getIntVals (Proxy @(DivMod 29 5)) `shouldBe` (29 `divMod` 5)
    it "calculates divMod for a positive integer and a natural" $
      getIntVals (Proxy @(DivMod (Pos 29) 5)) `shouldBe` (29 `divMod` 5)
    it "calculates divMod for a negative integer and a natural" $
      getIntVals (Proxy @(DivMod (Neg 29) 5)) `shouldBe` ((-29) `divMod` 5)
  describe "div" $ do
    it "calculates div for two naturals" $
      intVal (Proxy @(Div 29 5)) `shouldBe` (29 `div` 5)
    it "calculates div for a positive integer and a natural" $
      intVal (Proxy @(Div (Pos 29) 5)) `shouldBe` (29 `div` 5)
    it "calculates div for a negative integer and a natural" $
      intVal (Proxy @(Div (Neg 29) 5)) `shouldBe` ((-29) `div` 5)
  describe "mod" $ do
    it "calculates mod for two naturals" $
      intVal (Proxy @(Mod 29 5)) `shouldBe` (29 `mod` 5)
    it "calculates mod for a positive integer and a natural" $
      intVal (Proxy @(Mod (Pos 29) 5)) `shouldBe` (29 `mod` 5)
    it "calculates mod for a negative integer and a natural" $
      intVal (Proxy @(Mod (Neg 29) 5)) `shouldBe` ((-29) `mod` 5)

quotRemTests :: Spec
quotRemTests = do
  describe "quotRem" $ do
    it "calculates quotRem for two naturals" $
      getIntVals (Proxy @(QuotRem 29 5)) `shouldBe` (29 `quotRem` 5)
    it "calculates quotRem for a positive integer and a natural" $
      getIntVals (Proxy @(QuotRem (Pos 29) 5)) `shouldBe` (29 `quotRem` 5)
    it "calculates quotRem for a negative integer and a natural" $
      getIntVals (Proxy @(QuotRem (Neg 29) 5)) `shouldBe` ((-29) `quotRem` 5)
  describe "quot" $ do
    it "calculates quot for two naturals" $
      intVal (Proxy @(Quot 29 5)) `shouldBe` (29 `quot` 5)
    it "calculates quot for a positive integer and a natural" $
      intVal (Proxy @(Quot (Pos 29) 5)) `shouldBe` (29 `quot` 5)
    it "calculates quot for a negative integer and a natural" $
      intVal (Proxy @(Quot (Neg 29) 5)) `shouldBe` ((-29) `quot` 5)
  describe "rem" $ do
    it "calculates rem for two naturals" $
      intVal (Proxy @(Rem 29 5)) `shouldBe` (29 `rem` 5)
    it "calculates rem for a positive integer and a natural" $
      intVal (Proxy @(Rem (Pos 29) 5)) `shouldBe` (29 `rem` 5)
    it "calculates rem for a negative integer and a natural" $
      intVal (Proxy @(Rem (Neg 29) 5)) `shouldBe` ((-29) `rem` 5)

gcdTests :: Spec
gcdTests = do
  describe "gcd" $ do
    it "calculates gcd for two naturals" $
      intVal (Proxy @(GCD 50 40)) `shouldBe` 10
    it "calculates gcd for a positive int and a natural" $
      intVal (Proxy @(GCD (Pos 50) 40)) `shouldBe` 10
    it "calculates gcd for a natural and a positive int" $
      intVal (Proxy @(GCD 50 (Pos 40))) `shouldBe` 10
    it "calculates gcd for a negative int and a natural" $
      intVal (Proxy @(GCD (Neg 50) 40)) `shouldBe` 10
    it "calculates gcd for a natural and a negative int" $
      intVal (Proxy @(GCD 50 (Neg 40))) `shouldBe` 10
    it "calculates gcd for two positive integers" $
      intVal (Proxy @(GCD (Pos 50) (Pos 40))) `shouldBe` 10
    it "calculates gcd for a positive and a negative int" $
      intVal (Proxy @(GCD (Pos 50) (Neg 40))) `shouldBe` 10
    it "calculates gcd for a negative and a positive int" $
      intVal (Proxy @(GCD (Neg 50) (Pos 40))) `shouldBe` 10
    it "calculates gcd for two negative integers" $
      intVal (Proxy @(GCD (Neg 50) (Neg 40))) `shouldBe` 10

class RawRational (r :: Rat) where
  rawRatVals :: Proxy r -> (Integer, Integer)

instance forall n d. (KnownInt n, KnownNat d) => RawRational (n ':% d) where
  rawRatVals _ = (intVal (Proxy @n), natVal (Proxy @d))

simplifyTests :: Spec
simplifyTests = do
  describe "simplify" $ do
    it "acts as a no-op for an already-simplified fraction" $
      rawRatVals (Proxy @(Simplify (2 :% 3))) `shouldBe` (2, 3)
    it "reduces an unreduced fraction" $
      rawRatVals (Proxy @(Simplify (4 :% 6))) `shouldBe` (2, 3)
    it "no-ops for an already-simplified (int,int) fraction" $
      rawRatVals (Proxy @(Simplify ((Pos 2) :% 3))) `shouldBe` (2, 3)
    it "no-opts for an already-simplified (neg,int) fraction" $
      rawRatVals (Proxy @(Simplify ((Neg 2) :% 3))) `shouldBe` ((-2), 3)
    it "reduces an (int,int) unreduced fraction" $
      rawRatVals (Proxy @(Simplify ((Pos 4) :% 6))) `shouldBe` (2, 3)
    it "reduces a (neg, int) unreduced fraction" $
      rawRatVals (Proxy @(Simplify ((Neg 4) :% 6))) `shouldBe` ((-2), 3)
