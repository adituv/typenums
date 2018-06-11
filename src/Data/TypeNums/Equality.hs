{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE Safe            #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Data.TypeNums.Equality where

import qualified Data.Type.Equality as DTE

type (a :: k) == (b :: k) = ((DTE.==) a b) ~ 'True

type (a :: k) /= (b :: k) = ((DTE.==) a b) ~ 'False
