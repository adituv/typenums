{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestUtil where

import Control.Monad(unless)
import Data.Maybe(isJust)
import Data.Type.Equality
import Type.Reflection

import Test.Hspec

class DemoteBool (a :: Bool) where
  demote :: Bool

instance DemoteBool 'True where
  demote = True

instance DemoteBool 'False where
  demote = False

-- HasCallStack constraint is required to have the location reported as the callsite rather than here
typesShouldBeEqual :: forall k1 k2 (a :: k1) (b :: k2) (proxy :: (forall k. k -> *)).
                      (Typeable k1, Typeable k2, Typeable a, Typeable b, HasCallStack)
                   => proxy a
                   -> proxy b
                   -> Expectation
typesShouldBeEqual pa pb = unless (isJust $ eqTypeRep typeOfA typeOfB) (expectationFailure typesUnequalMessage)
  where
    typeOfA = typeRep @a
    typeOfB = typeRep @b
    typesUnequalMessage = "expected type: " ++ show typeOfB ++ "\n but got type: " ++ show typeOfA