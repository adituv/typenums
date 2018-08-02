{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test where

import Data.TypeNums
import Data.TypeNums.Arithmetic.Internal(AddK, Add)

data InfiniteK = Infinite

type instance AddK Nat       InfiniteK = InfiniteK
type instance AddK InfiniteK Nat       = InfiniteK
type instance AddK InfiniteK InfiniteK = InfiniteK

type instance Add (x :: Nat) Infinite   = Infinite
type instance Add Infinite   (y :: Nat) = Infinite
type instance Add Infinite   Infinite   = Infinite
