{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Test.Count
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-----------------------------------------------------------------------------

module Test.Count (tests) where

--------------------------------------------------------------------------------

import Test.HUnit ((~:), Test)

import Data.Int
import Data.Word

import Test.QuickCheck.Instances.Int ()
import Test.QuickCheck.Instances.Word ()

import qualified Properties.Count as Prop
import Util

--------------------------------------------------------------------------------

tests :: Test
tests =
  "Count" ~:
    [ countDigits_int_show
    ]

--------------------------------------------------------------------------------

countDigits_int_show :: Test
countDigits_int_show =
  "countDigits_int_show" ~:
    [ test_qc_typed1 (undefined :: Int)    Prop.countDigits_int_show
    , test_qc_typed1 (undefined :: Int8)   Prop.countDigits_int_show
    , test_qc_typed1 (undefined :: Int16)  Prop.countDigits_int_show
    , test_qc_typed1 (undefined :: Int32)  Prop.countDigits_int_show
    , test_qc_typed1 (undefined :: Int64)  Prop.countDigits_int_show
    , test_qc_typed1 (undefined :: Word)   Prop.countDigits_int_show
    , test_qc_typed1 (undefined :: Word8)  Prop.countDigits_int_show
    , test_qc_typed1 (undefined :: Word16) Prop.countDigits_int_show
    , test_qc_typed1 (undefined :: Word32) Prop.countDigits_int_show
    , test_qc_typed1 (undefined :: Word64) Prop.countDigits_int_show
    ]

