{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Util
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-----------------------------------------------------------------------------

module Util where

--------------------------------------------------------------------------------

import Test.HUnit ((~:), Test)
import Test.QuickCheck (quickCheckResult, Testable, Arbitrary(..))
import Test.QuickCheck.Test (isSuccess)
import Control.Applicative ((<$>), (<*>))

import Data.Generics (Data, dataTypeName, dataTypeOf)

import Digits (Digits(..))

--------------------------------------------------------------------------------

instance (Arbitrary n) => Arbitrary (Digits n) where
  arbitrary = Digits <$> arbitrary <*> arbitrary <*> arbitrary

--------------------------------------------------------------------------------

dataTypeNameOf :: (Data a) => a -> String
dataTypeNameOf = dataTypeName . dataTypeOf

(~|:) :: (Testable prop) => String -> prop -> Test
lbl ~|: t = lbl ~: quickCheckResult t >>= return . isSuccess

eqNegBound :: (Bounded n, Ord n, Num n) => n -> Bool
eqNegBound n = n == minBound && n < 0

abs' :: (Bounded n, Ord n, Num n) => n -> n
abs' n | eqNegBound n = minBound
       | otherwise    = negate n

--------------------------------------------------------------------------------

test_qc_typed1 :: (Show n, Data n, Arbitrary n, Testable prop) => n -> (n -> prop) -> Test
test_qc_typed1 x p = dataTypeNameOf x ~|: p

test_qc_typed2 :: (Show n, Data n, Arbitrary n, Testable prop) => n -> (n -> n -> prop) -> Test
test_qc_typed2 x p = dataTypeNameOf x ~|: p

test_qc_typed3 :: (Show n, Data n, Arbitrary n, Testable prop) => n -> (Digits n -> prop) -> Test
test_qc_typed3 x p = dataTypeNameOf x ~|: p

