{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Data.TypeNums
import Test.Hspec

import Data.Proxy
import Data.Ratio
import GHC.Exts(Proxy#, proxy#)

main :: IO ()
main = hspec $ do
  intTests
  ratTests
  -- Can't really test constraint comparisons as it seems that
  -- should-not-typecheck doesn't handle constraints
  arithTests

intTests :: Spec
intTests = do
  describe "intVal" $ do
    it "correctly gets the value of a natural number" $
      intVal (Proxy @3) `shouldBe` 3
    it "correctly gets the value of zero" $
      intVal (Proxy @0) `shouldBe` 0
    it "correctly gets the value of Neg 0" $
      intVal (Proxy @('Neg 0)) `shouldBe` 0
    it "correctly gets the value of a negative integer" $
      intVal (Proxy @('Neg 10)) `shouldBe` (-10)
  describe "intVal'" $ do
    it "correctly gets the value of a natural number" $
      intVal' (proxy# :: Proxy# 3) `shouldBe` 3
    it "correctly gets the value of zero" $
      intVal' (proxy# :: Proxy# 0) `shouldBe` 0
    it "correctly gets the value of Neg 0" $
      intVal' (proxy# :: Proxy# ('Neg 0)) `shouldBe` 0
    it "correctly gets the value of a negative integer" $
      intVal' (proxy# :: Proxy# ('Neg 10)) `shouldBe` (-10)

ratTests :: Spec
ratTests = do
  describe "ratVal" $ do
    it "correctly gets the value of a natural number" $
      ratVal (Proxy @3) `shouldBe` 3
    it "correctly gets the value of zero" $
      ratVal (Proxy @0) `shouldBe` 0
    it "correctly gets the value of Neg 0" $
      ratVal (Proxy @('Neg 0)) `shouldBe` 0
    it "correctly gets the value of a negative integer" $
      ratVal (Proxy @('Neg 10)) `shouldBe` (-10)
    it "correctly gets the value of a positive rational" $
      ratVal (Proxy @(3 ':% 2)) `shouldBe` 3 % 2
    it "correctly gets the value of a negative rational" $
      ratVal (Proxy @(11 ':% 8)) `shouldBe` 11 % 8
  describe "ratVal'" $ do
    it "correctly gets the value of a natural number" $
      ratVal' (proxy# :: Proxy# 3) `shouldBe` 3
    it "correctly gets the value of zero" $
      ratVal' (proxy# :: Proxy# 0) `shouldBe` 0
    it "correctly gets the value of Neg 0" $
      ratVal' (proxy# :: Proxy# ('Neg 0)) `shouldBe` 0
    it "correctly gets the value of a negative integer" $
      ratVal' (proxy# :: Proxy# ('Neg 10)) `shouldBe` (-10)
    it "correctly gets the value of a positive rational" $
      ratVal' (proxy# :: Proxy# (3 ':% 2)) `shouldBe` 3 % 2
    it "correctly gets the value of a negative rational" $
      ratVal' (proxy# :: Proxy# (11 ':% 8)) `shouldBe` 11 % 8

arithTests :: Spec
arithTests = do
  describe "multiplication" $ do
    it "correctly multiplies two natural numbers" $
      ratVal (Proxy @(3 * 5)) `shouldBe` 15
    it "correctly multiplies a negative integer and a natural" $
      ratVal (Proxy @((Neg 4) * 3)) `shouldBe` (-12)
    it "correctly multiplies a natural and a negative integer" $
      ratVal (Proxy @(7 * (Neg 8))) `shouldBe` (-56)
    it "correctly multiplies two negative integers" $
      ratVal (Proxy @((Neg 5) * (Neg 6))) `shouldBe` 30
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
