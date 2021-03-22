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
import Data.Proxy(Proxy(..))
import Test.Hspec(Spec, describe, it, shouldBe)

import TestUtil(typesShouldBeEqual)

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
  expTests
  roundingTests
  logTests

additionTests :: Spec
additionTests =
  describe "addition" $ do
    it "correctly adds two natural numbers" $
      typesShouldBeEqual (Proxy @(3 + 5)) (Proxy @8)
    it "correctly adds a negative integer and a natural" $
      typesShouldBeEqual (Proxy @(Neg 4 + 3)) (Proxy @(Neg 1))
    it "correctly adds a natural and a negative integer" $
      typesShouldBeEqual (Proxy @(8 + Neg 7)) (Proxy @(Pos 1))
    it "correctly adds two negative integers" $
      typesShouldBeEqual (Proxy @(Neg 5 + Neg 6)) (Proxy @(Neg 11))
    it "correctly adds two positive integers" $
      typesShouldBeEqual (Proxy @(Pos 5 + Pos 6)) (Proxy @(Pos 11))
    it "correctly adds a negative integer and a positive integer" $
      typesShouldBeEqual (Proxy @(Neg 3 + Pos 5)) (Proxy @(Pos 2))
    it "correctly adds a positive integer and a negative integer" $
      typesShouldBeEqual (Proxy @(Pos 7 + Neg 8)) (Proxy @(Neg 1))
    it "correctly adds two rationals" $
      typesShouldBeEqual (Proxy @(2:%3 + 5:%7)) (Proxy @(Pos 29 :% 21))
    it "correctly adds a rational with a natural" $
      typesShouldBeEqual (Proxy @(2:%3 + 2)) (Proxy @(Pos 8 :% 3))
    it "correctly adds a natural with a rational" $
      typesShouldBeEqual (Proxy @(2 + 2:%3)) (Proxy @(Pos 8 :% 3))
    it "correctly adds a rational with a negative integer" $
      typesShouldBeEqual (Proxy @(2':%3 + Neg 2)) (Proxy @(Neg 4 :% 3))
    it "correctly adds a negative integer with a rational" $
      typesShouldBeEqual (Proxy @(Neg 2 + 2:%3)) (Proxy @(Neg 4 :% 3))
    it "correctly adds a rational with a positive integer" $
      typesShouldBeEqual (Proxy @(2:%3 + Pos 2)) (Proxy @(Pos 8 :% 3))
    it "correctly adds a positive integer with a rational" $
      typesShouldBeEqual (Proxy @(Pos 2 + 2:%3)) (Proxy @(Pos 8 :% 3))
    it "correctly simplifies the result of adding two rationals" $
      typesShouldBeEqual (Proxy @(3:%4 + 3:%4)) (Proxy @(Pos 3 :% 2))

subtractionTests :: Spec
subtractionTests =
  describe "subtraction" $ do
    it "correctly subtracts two natural numbers" $
      typesShouldBeEqual (Proxy @(5-3)) (Proxy @2)
    it "correctly subtracts a negative integer and a natural" $
      typesShouldBeEqual (Proxy @(Neg 4 - 3)) (Proxy @(Neg 7))
    it "correctly subtracts a natural and a negative integer" $
      typesShouldBeEqual (Proxy @(8 - Neg 7)) (Proxy @(Pos 15))
    it "correctly subtracts two negative integers" $
      typesShouldBeEqual (Proxy @(Neg 5 - Neg 6)) (Proxy @(Pos 1))
    it "correctly subtracts two positive integers" $
      typesShouldBeEqual (Proxy @(Pos 5 - Pos 6)) (Proxy @(Neg 1))
    it "correctly subtracts a negative integer and a positive integer" $
      typesShouldBeEqual (Proxy @(Neg 3 - Pos 5)) (Proxy @(Neg 8))
    it "correctly subtracts a positive integer and a negative integer" $
      typesShouldBeEqual (Proxy @(Pos 7 - Neg 8)) (Proxy @(Pos 15))
    it "correctly subtracts two rationals with a negative result" $
      typesShouldBeEqual (Proxy @(2:%3 - 5:%7)) (Proxy @(Neg 1 :% 21))
    it "correctly subtracts two rationals with a positive result" $
      typesShouldBeEqual (Proxy @(5:%7 - 2:%3)) (Proxy @(Pos 1 :% 21))
    it "correctly subtracts a rational with a natural" $
      typesShouldBeEqual (Proxy @(2:%3 - 2)) (Proxy @(Neg 4 :% 3))
    it "correctly subtracts a natural with a rational" $
      typesShouldBeEqual (Proxy @(2 - 2:%3)) (Proxy @(Pos 4 :% 3))
    it "correctly subtracts a rational with a negative integer" $
      typesShouldBeEqual (Proxy @(2:%3 - Neg 2)) (Proxy @(Pos 8 :% 3))
    it "correctly subtracts a negative integer with a rational" $
      typesShouldBeEqual (Proxy @(Neg 2 - 2:%3)) (Proxy @(Neg 8 :% 3))
    it "correctly subtracts a rational with a positive integer" $
      typesShouldBeEqual (Proxy @(2:%3 - Pos 2)) (Proxy @(Neg 4 :% 3))
    it "correctly subtracts a positive integer with a rational" $
      typesShouldBeEqual (Proxy @(Pos 2 - 2:%3)) (Proxy @(Pos 4 :% 3))
    it "correctly simplifies the result of subtracting two rationals" $
      typesShouldBeEqual (Proxy @(4:%9 - 1:%9)) (Proxy @(Pos 1:%3))

multiplicationTests :: Spec
multiplicationTests =
  describe "multiplication" $ do
    it "correctly multiplies two natural numbers" $
      typesShouldBeEqual (Proxy @(3 * 5)) (Proxy @15)
    it "correctly multiplies a negative integer and a natural" $
      typesShouldBeEqual (Proxy @(Neg 4 * 3)) (Proxy @(Neg 12))
    it "correctly multiplies a natural and a negative integer" $
      typesShouldBeEqual (Proxy @(7 * Neg 8)) (Proxy @(Neg 56))
    it "correctly multiplies two negative integers" $
      typesShouldBeEqual (Proxy @(Neg 5 * Neg 6)) (Proxy @(Pos 30))
    it "correctly multiplies two positive integers" $
      typesShouldBeEqual (Proxy @(Pos 5 * Pos 6)) (Proxy @(Pos 30))
    it "correctly multiplies a negative integer and a positive integer" $
      typesShouldBeEqual (Proxy @(Neg 4 * Pos 3)) (Proxy @(Neg 12))
    it "correctly multiplies a positive integer and a negative integer" $
      typesShouldBeEqual (Proxy @(Pos 7 * Neg 8)) (Proxy @(Neg 56))
    it "correctly multiplies two rationals" $
      typesShouldBeEqual (Proxy @(2:%3 * 5:%7)) (Proxy @(Pos 10 :% 21))
    it "correctly multiplies a rational with a natural" $
      typesShouldBeEqual (Proxy @(2:%3 * 2)) (Proxy @(Pos 4 :% 3))
    it "correctly multiplies a natural with a rational" $
      typesShouldBeEqual (Proxy @(2 * 2:%3)) (Proxy @(Pos 4 :% 3))
    it "correctly multiplies a rational with a negative integer" $
      typesShouldBeEqual (Proxy @(2:%3 * Neg 2)) (Proxy @(Neg 4 :% 3))
    it "correctly multiplies a negative integer with a rational" $
      typesShouldBeEqual (Proxy @(Neg 2 * 2:%3)) (Proxy @(Neg 4 :% 3))
    it "correctly multiplies a rational with a positive integer" $
      typesShouldBeEqual (Proxy @(2:%3 * Pos 2)) (Proxy @(Pos 4 :% 3))
    it "correctly multiplies a positive integer with a rational" $
      typesShouldBeEqual (Proxy @(Pos 2 * 2:%3)) (Proxy @(Pos 4 :% 3))
    it "correctly simplifies the result of multiplying two rationals" $
      typesShouldBeEqual (Proxy @(2:%3 * 3:%5)) (Proxy @(Pos 2 :% 5))

absTests :: Spec
absTests = do
  describe "abs" $ do
    it "Acts as a no-op on a natural" $
      typesShouldBeEqual (Proxy @(Abs 3)) (Proxy @3)
    it "Acts as a no-op on a positive int" $
      typesShouldBeEqual (Proxy @(Abs (Pos 3))) (Proxy @(Pos 3))
    it "Negates a negative int" $
      typesShouldBeEqual (Proxy @(Abs (Neg 3))) (Proxy @(Pos 3))
    it "Normalises a positive rational with natural numerator" $
      typesShouldBeEqual (Proxy @(Abs (2:%3))) (Proxy @(Pos 2 :% 3))
    it "Normalises a positive rational with signed positive numerator" $
      typesShouldBeEqual (Proxy @(Abs (Pos 4 :% 6))) (Proxy @(Pos 2 :% 3))
    it "Negates a negative rational" $
      typesShouldBeEqual (Proxy @(Abs (Neg 2 :% 3))) (Proxy @(Pos 2 :% 3))

negateTests :: Spec
negateTests = do
  describe "negate" $ do
    it "Turns a natural into Neg" $
      typesShouldBeEqual (Proxy @(Negate 3)) (Proxy @(Neg 3))
    it "Turns Pos into Neg" $
      typesShouldBeEqual (Proxy @(Negate (Pos 3))) (Proxy @(Neg 3))
    it "Turns Neg into Pos" $
      typesShouldBeEqual (Proxy @(Negate (Neg 3))) (Proxy @(Pos 3))
    it "Negates a rational with natural numerator" $
      typesShouldBeEqual (Proxy @(Negate (3 :% 4))) (Proxy @(Neg 3 :% 4))
    it "Negates a rational with positive numerator" $
      typesShouldBeEqual (Proxy @(Negate (Pos 4 :% 5))) (Proxy @(Neg 4 :% 5))
    it "Negates a rational with negative numerator" $
      typesShouldBeEqual (Proxy @(Negate (Neg 5 :% 4))) (Proxy @(Pos 5 :% 4))

recipTests :: Spec
recipTests = do
  describe "recip" $ do
    it "Inverts Nat / Nat" $
      typesShouldBeEqual (Proxy @(Recip (3 :% 2))) (Proxy @(Pos 2 :% 3))
    it "Inverts Pos / Nat" $
      typesShouldBeEqual (Proxy @(Recip (Pos 3 :% 2))) (Proxy @(Pos 2 :% 3))
    it "Inverts Neg / Nat" $
      typesShouldBeEqual (Proxy @(Recip (Neg 3 :% 2))) (Proxy @(Neg 2 :% 3))
    it "Inverts Nat" $
      typesShouldBeEqual (Proxy @(Recip 5)) (Proxy @(Pos 1 :% 5))
    it "Inverts a positive TInt" $
      typesShouldBeEqual (Proxy @(Recip (Pos 4))) (Proxy @(Pos 1 :% 4))
    it "Inverts a negative TInt" $
      typesShouldBeEqual (Proxy @(Recip (Neg 3))) (Proxy @(Neg 1 :% 3))

divisionTests :: Spec
divisionTests = do
  describe "ratdiv" $ do
    it "Divides two naturals" $
      typesShouldBeEqual (Proxy @(3 / 2)) (Proxy @(Pos 3 :% 2))
    it "Divides a positive integer by a natural" $
      typesShouldBeEqual (Proxy @(Pos 3 / 2)) (Proxy @(Pos 3 :% 2))
    it "Divides a negative integer by a natural" $
      typesShouldBeEqual (Proxy @(Neg 3 / 2)) (Proxy @(Neg 3 :% 2))
    it "Divides a natural by a positive integer" $
      typesShouldBeEqual (Proxy @(3 / Pos 2)) (Proxy @(Pos 3 :% 2))
    it "Divides a natural by a negative integer" $
      typesShouldBeEqual (Proxy @(3 / Neg 2)) (Proxy @(Neg 3 :% 2))
    it "Divides a positive integer by a negative integer" $
      typesShouldBeEqual (Proxy @(Pos 3 / Neg 2)) (Proxy @(Neg 3 :% 2))
    it "Divides a negative integer by a positive integer" $
      typesShouldBeEqual (Proxy @(Neg 3 / Pos 2)) (Proxy @(Neg 3 :% 2))
    it "Divides a negative integer by a negative integer" $
      typesShouldBeEqual (Proxy @(Neg 3 / Neg 2)) (Proxy @(Pos 3 :% 2))
    it "Divides a natural by a rational" $
      typesShouldBeEqual (Proxy @(5 / (2:%3))) (Proxy @(Pos 15 :% 2))
    it "Divides a positive integer by a rational" $
      typesShouldBeEqual (Proxy @(Pos 5 / 2:%3)) (Proxy @(Pos 15 :% 2))
    it "Divides a negative integer by a rational" $
      typesShouldBeEqual (Proxy @(Neg 5 / 2:%3)) (Proxy @(Neg 15 :% 2))
    it "Divides a negative integer by a negative rational" $
      typesShouldBeEqual (Proxy @(Neg 5 / (Neg 2:% 3))) (Proxy @(Pos 15 :% 2))
    it "Divides a rational by a natural" $
      typesShouldBeEqual (Proxy @(2:%3 / 5)) (Proxy @(Pos 2 :% 15))
    it "Divides a rational by a positive integer" $
      typesShouldBeEqual (Proxy @(2:%3 / Pos 5)) (Proxy @(Pos 2 :% 15))
    it "Divides a rational by a negative integer" $
      typesShouldBeEqual (Proxy @(2:%3 / Neg 5)) (Proxy @(Neg 2 :% 15))
    it "Divides a negative rational by a negative integer" $
      typesShouldBeEqual (Proxy @((Neg 2 :% 3) / Neg 5)) (Proxy @(Pos 2 :% 15))
    it "Divides a rational by a rational" $
      typesShouldBeEqual (Proxy @(2:%3 / 5:%7)) (Proxy @(Pos 14 :% 15))
    it "Simplifies the result of division" $
      typesShouldBeEqual (Proxy @(6 / 4)) (Proxy @(Pos 3 :% 2))

divModTests :: Spec
divModTests = do
  describe "divMod" $ do
    it "calculates divMod for two naturals" $
      typesShouldBeEqual (Proxy @(DivMod 29 5)) (Proxy @'(5,4))
    it "calculates divMod for a positive integer and a natural" $
      typesShouldBeEqual (Proxy @(DivMod (Pos 29) 5)) (Proxy @'(Pos 5, Pos 4))
    it "calculates divMod for a negative integer and a natural" $
      typesShouldBeEqual (Proxy @(DivMod (Neg 29) 5)) (Proxy @'(Neg 6, Pos 1))
    it "calculates divMod for a negative integer and 1" $
      typesShouldBeEqual (Proxy @(DivMod (Neg 3) 1)) (Proxy @'(Neg 3, Pos 0))
  describe "div" $ do
    it "calculates div for two naturals" $
      typesShouldBeEqual (Proxy @(Div 29 5)) (Proxy @5)
    it "calculates div for a positive integer and a natural" $
      typesShouldBeEqual (Proxy @(Div (Pos 29) 5)) (Proxy @(Pos 5))
    it "calculates div for a negative integer and a natural" $
      typesShouldBeEqual (Proxy @(Div (Neg 29) 5)) (Proxy @(Neg 6))
    it "divides a negative number by 1" $
      typesShouldBeEqual (Proxy @(Div (Neg 3) 1)) (Proxy @(Neg 3))
  describe "mod" $ do
    it "calculates mod for two naturals" $
      typesShouldBeEqual (Proxy @(Mod 29 5)) (Proxy @4)
    it "calculates mod for a positive integer and a natural" $
      typesShouldBeEqual (Proxy @(Mod (Pos 29) 5)) (Proxy @(Pos 4))
    it "calculates mod for a negative integer and a natural" $
      typesShouldBeEqual (Proxy @(Mod (Neg 29) 5)) (Proxy @(Pos 1))
    it "calculates mod for a negative integer and 1" $
      typesShouldBeEqual (Proxy @(Mod (Neg 3) 1)) (Proxy @(Pos 0))

quotRemTests :: Spec
quotRemTests = do
  describe "quotRem" $ do
    it "calculates quotRem for two naturals" $
      typesShouldBeEqual (Proxy @(QuotRem 29 5)) (Proxy @'(5,4))
    it "calculates quotRem for a positive integer and a natural" $
      typesShouldBeEqual (Proxy @(QuotRem (Pos 29) 5)) (Proxy @'(Pos 5, Pos 4))
    it "calculates quotRem for a negative integer and a natural" $
      typesShouldBeEqual (Proxy @(QuotRem (Neg 29) 5)) (Proxy @'(Neg 5, Neg 4))
    it "calculates quotRem for a negative integer and 1" $
      typesShouldBeEqual (Proxy @(QuotRem (Neg 3) 1)) (Proxy @'(Neg 3, Pos 0))
  describe "quot" $ do
    it "calculates quot for two naturals" $
      typesShouldBeEqual (Proxy @(Quot 29 5)) (Proxy @5)
    it "calculates quot for a positive integer and a natural" $
      typesShouldBeEqual (Proxy @(Quot (Pos 29) 5)) (Proxy @(Pos 5))
    it "calculates quot for a negative integer and a natural" $
      typesShouldBeEqual (Proxy @(Quot (Neg 29) 5)) (Proxy @(Neg 5))
    it "calculates quotRem for a negative integer and 1" $
      typesShouldBeEqual (Proxy @(Quot (Neg 3) 1)) (Proxy @(Neg 3))
  describe "rem" $ do
    it "calculates rem for two naturals" $
      typesShouldBeEqual (Proxy @(Rem 29 5)) (Proxy @4)
    it "calculates rem for a positive integer and a natural" $
      typesShouldBeEqual (Proxy @(Rem (Pos 29) 5)) (Proxy @(Pos 4))
    it "calculates rem for a negative integer and a natural" $
      typesShouldBeEqual (Proxy @(Rem (Neg 29) 5)) (Proxy @(Neg 4))
    it "calculates rem for a negative integer and 1" $
      typesShouldBeEqual (Proxy @(Rem (Neg 3) 1)) (Proxy @(Pos 0))

gcdTests :: Spec
gcdTests = do
  describe "gcd" $ do
    it "calculates gcd for two naturals" $
      typesShouldBeEqual (Proxy @(GCD 50 40)) (Proxy @10)
    it "calculates gcd for a positive int and a natural" $
      typesShouldBeEqual (Proxy @(GCD (Pos 50) 40)) (Proxy @10)
    it "calculates gcd for a natural and a positive int" $
      typesShouldBeEqual (Proxy @(GCD 50 (Pos 40))) (Proxy @10)
    it "calculates gcd for a negative int and a natural" $
      typesShouldBeEqual (Proxy @(GCD (Neg 50) 40)) (Proxy @10)
    it "calculates gcd for a natural and a negative int" $
      typesShouldBeEqual (Proxy @(GCD 50 (Neg 40))) (Proxy @10)
    it "calculates gcd for two positive integers" $
      typesShouldBeEqual (Proxy @(GCD (Pos 50) (Pos 40))) (Proxy @10)
    it "calculates gcd for a positive and a negative int" $
      typesShouldBeEqual (Proxy @(GCD (Pos 50) (Neg 40))) (Proxy @10)
    it "calculates gcd for a negative and a positive int" $
      typesShouldBeEqual (Proxy @(GCD (Neg 50) (Pos 40))) (Proxy @10)
    it "calculates gcd for two negative integers" $
      typesShouldBeEqual (Proxy @(GCD (Neg 50) (Neg 40))) (Proxy @10)

simplifyTests :: Spec
simplifyTests = do
  describe "simplify" $ do
    it "acts as a no-op for an already-simplified fraction" $
      typesShouldBeEqual (Proxy @(Simplify (2 :% 3))) (Proxy @(Pos 2 :% 3))
    it "reduces an unreduced fraction" $
      typesShouldBeEqual (Proxy @(Simplify (4 :% 6))) (Proxy @(Pos 2 :% 3))
    it "no-ops for an already-simplified positive TInt fraction" $
      typesShouldBeEqual (Proxy @(Pos 2 :% 3)) (Proxy @(Pos 2 :% 3))
    it "no-opts for an already-simplified negative TInt fraction" $
      typesShouldBeEqual (Proxy @(Simplify (Neg 2 :% 3))) (Proxy @(Neg 2 :% 3))
    it "reduces a positive TInt unreduced fraction" $
      typesShouldBeEqual (Proxy @(Simplify (Pos 4 :% 6))) (Proxy @(Pos 2 :% 3))
    it "reduces a (neg, int) unreduced fraction" $
      typesShouldBeEqual (Proxy @(Simplify (Neg 4 :% 6))) (Proxy @(Neg 2 :% 3))

expTests :: Spec
expTests = do
  describe "exp" $ do
    it "calculates x^0 for natural x" $
      typesShouldBeEqual (Proxy @(3^0)) (Proxy @1)
    it "calculates x^(+0) for natural x" $
      typesShouldBeEqual (Proxy @(3^Pos 0)) (Proxy @(Pos 1 :% 1))
    it "calculates x^(-0) for natural x" $
      typesShouldBeEqual (Proxy @(3^Neg 0)) (Proxy @(Pos 1 :% 1))
    it "calculates x^0 for integer x" $
      typesShouldBeEqual (Proxy @(Pos 3 ^ 0)) (Proxy @(Pos 1))
    it "calculates x^(+0) for integer x" $
      typesShouldBeEqual (Proxy @(Pos 3 ^ Pos 0)) (Proxy @(Pos 1 :% 1))
    it "calculates x^(Neg 0) for integer x" $
      typesShouldBeEqual (Proxy @(Pos 3 ^ Neg 0)) (Proxy @(Pos 1 :% 1))
    it "calculates x^0 for rational x" $
      typesShouldBeEqual (Proxy @(3:%5 ^ 0)) (Proxy @(Pos 1 :% 1))
    it "calculates x^(+0) for rational x" $
      typesShouldBeEqual (Proxy @(3:%5 ^ Pos 0)) (Proxy @(Pos 1 :% 1))
    it "calculates x^(Neg 0) for rational x" $
      typesShouldBeEqual (Proxy @(3:%5 ^ Neg 0)) (Proxy @(Pos 1 :% 1))
    it "calculates x^y for natural x,y" $
      typesShouldBeEqual (Proxy @(5^3)) (Proxy @125)
    it "calculates (+x)^y for natural y" $
      typesShouldBeEqual (Proxy @(Pos 5 ^ 3)) (Proxy @(Pos 125))

roundingTests :: Spec
roundingTests = do
  describe "truncate" $ do
    it "acts as a no-op on a natural" $
      typesShouldBeEqual (Proxy @(Truncate 3)) (Proxy @(Pos 3))
    it "acts as a no-op on a positive int" $
      typesShouldBeEqual (Proxy @(Truncate (Pos 3))) (Proxy @(Pos 3))
    it "acts as a no-op on a negative int" $
      typesShouldBeEqual (Proxy @(Truncate (Neg 3))) (Proxy @(Neg 3))
    it "acts as a no-op on (x :% 1) for natural x" $
      typesShouldBeEqual (Proxy @(Truncate (3 :% 1))) (Proxy @(Pos 3))
    it "acts as a no-op on (x :% 1) for positive x" $
      typesShouldBeEqual (Proxy @(Truncate (Pos 3 :% 1))) (Proxy @(Pos 3))
    it "acts as a no-op on (x :% 1) for negative x" $
      typesShouldBeEqual (Proxy @(Truncate (Neg 3 :% 1))) (Proxy @(Neg 3))
    it "rounds a positive rational towards 0" $
      typesShouldBeEqual (Proxy @(Truncate (25 :% 3))) (Proxy @(Pos 8))
    it "rounds a negative rational towards 0" $
      typesShouldBeEqual (Proxy @(Truncate (Neg 25 ':% 3))) (Proxy @(Neg 8))
  describe "floor" $ do
    it "acts as a no-op on a natural" $
      typesShouldBeEqual (Proxy @(Floor 3)) (Proxy @(Pos 3))
    it "acts as a no-op on a positive int" $
      typesShouldBeEqual (Proxy @(Floor (Pos 3))) (Proxy @(Pos 3))
    it "acts as a no-op on a negative int" $
      typesShouldBeEqual (Proxy @(Floor (Neg 3))) (Proxy @(Neg 3))
    it "acts as a no-op on (x :% 1) for natural x" $
      typesShouldBeEqual (Proxy @(Floor (3 :% 1))) (Proxy @(Pos 3))
    it "acts as a no-op on (x :% 1) for positive x" $
      typesShouldBeEqual (Proxy @(Floor (Pos 3 :% 1))) (Proxy @(Pos 3))
    it "acts as a no-op on (x :% 1) for negative x" $
      typesShouldBeEqual (Proxy @(Floor (Neg 3 :% 1))) (Proxy @(Neg 3))
    it "rounds a positive rational towards -inf" $
      typesShouldBeEqual (Proxy @(Floor (25 ':% 3))) (Proxy @(Pos 8))
    it "rounds a negative rational towards -inf" $
      typesShouldBeEqual (Proxy @(Floor (Neg 25 ':% 3))) (Proxy @(Neg 9))
  describe "ceiling" $ do
    it "acts as a no-op on a natural" $
      typesShouldBeEqual (Proxy @(Ceiling 3)) (Proxy @(Pos 3))
    it "acts as a no-op on a positive int" $
      typesShouldBeEqual (Proxy @(Ceiling (Pos 3))) (Proxy @(Pos 3))
    it "acts as a no-op on a negative int" $
      typesShouldBeEqual (Proxy @(Ceiling (Neg 3))) (Proxy @(Neg 3))
    it "acts as a no-op on (x :% 1) for natural x" $
      typesShouldBeEqual (Proxy @(Ceiling (3 ':% 1))) (Proxy @(Pos 3))
    it "acts as a no-op on (x :% 1) for positive x" $
      typesShouldBeEqual (Proxy @(Ceiling (Pos 3 ':% 1))) (Proxy @(Pos 3))
    it "acts as a no-op on (x :% 1) for negative x" $
      typesShouldBeEqual (Proxy @(Ceiling (Neg 3 ':% 1))) (Proxy @(Neg 3))
    it "rounds a positive rational towards +inf" $
      typesShouldBeEqual (Proxy @(Ceiling (25 ':% 3))) (Proxy @(Pos 9))
    it "rounds a negative rational towards +inf" $
      typesShouldBeEqual (Proxy @(Ceiling (Neg 25 ':% 3))) (Proxy @(Neg 8))

logTests :: Spec
logTests = do
  describe "intlog" $ do
    it "calculates log_2(8)" $
      typesShouldBeEqual (Proxy @(IntLog 2 8)) (Proxy @(Pos 3))
    it "calculates log_2(3)" $
      typesShouldBeEqual (Proxy @(IntLog 2 3)) (Proxy @(Pos 1))
    it "calculates log_3(8)" $
      typesShouldBeEqual (Proxy @(IntLog 3 8)) (Proxy @(Pos 1))
    it "calculates log_3(9)" $
      typesShouldBeEqual (Proxy @(IntLog 3 9)) (Proxy @(Pos 2))
    it "calcuclates log_3(2)" $
      typesShouldBeEqual (Proxy @(IntLog 3 2)) (Proxy @(Pos 0))
    it "calculates log_2(1/2)" $
      typesShouldBeEqual (Proxy @(IntLog 2 (1 :% 2))) (Proxy @(Neg 1))
    it "calculates log_2(2/3)" $
      typesShouldBeEqual (Proxy @(IntLog 2 (2 :% 3))) (Proxy @(Neg 1))
    it "calculates log_2(1/3)" $
      typesShouldBeEqual (Proxy @(IntLog 2 (1 :% 3))) (Proxy @(Neg 2))
    it "calculates log_3(1/27)" $
      typesShouldBeEqual (Proxy @(IntLog 3 (1 :% 27))) (Proxy @(Neg 3))
    it "calculates log_3(1/2)" $
      typesShouldBeEqual (Proxy @(IntLog 3 (1 :% 2))) (Proxy @(Neg 1))
    it "calculates log_3(3/5)" $
      typesShouldBeEqual (Proxy @(IntLog 3 (3 :% 5))) (Proxy @(Neg 1))
    it "calculates log_6(3/5)" $
      typesShouldBeEqual (Proxy @(IntLog 6 (3 :% 5))) (Proxy @(Neg 1))
