{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
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
