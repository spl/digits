{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Test.EnumInt
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-----------------------------------------------------------------------------

module Test.EnumInt (tests) where

--------------------------------------------------------------------------------

import Test.HUnit ((~:), (~?), Test)
import Test.QuickCheck

import Data.Generics (Data)

import Data.Int
import Data.Word

import Test.QuickCheck.Instances.Int ()
import Test.QuickCheck.Instances.Word ()

import Digits.EnumInt (EnumIntDigits)
import qualified Properties.EnumInt as Prop
import Util

--------------------------------------------------------------------------------

tests :: Test
tests =
  "EnumInt" ~:
    [ iso
    , iso_bounds
    , fromIntDigits_eq
    , toIntDigits_eq
    , toDigits_int_show
    ]

--------------------------------------------------------------------------------

iso :: Test
iso =
  "iso" ~:
    [ qc (undefined :: Int8)    Prop.iso
    , qc (undefined :: Int16)   Prop.iso
    , qc (undefined :: Int32)   Prop.iso
    , qc (undefined :: Int64)   Prop.iso
    , qc (undefined :: Word)    Prop.iso
    , qc (undefined :: Word8)   Prop.iso
    , qc (undefined :: Word16)  Prop.iso
    , qc (undefined :: Word32)  Prop.iso
    , qc (undefined :: Word64)  Prop.iso
    , qc (undefined :: Integer) Prop.iso
    ]
  where
    qc :: (Show n, Data n, Arbitrary n, Testable prop) => n -> (n -> n -> prop) -> Test
    qc x p = dataTypeNameOf x ~|: p

iso_min_max :: (EnumIntDigits n, Data n, Bounded n) => n -> Test
iso_min_max base =
  dataTypeNameOf base ~:
    [ Prop.iso base minBound ~? "minBound"
    , Prop.iso base maxBound ~? "maxBound"
    ]

iso_bounds :: Test
iso_bounds =
  "bounds" ~:
    [ iso_min_max (10 :: Int8)
    , iso_min_max (10 :: Int16)
    , iso_min_max (10 :: Int32)
    , iso_min_max (10 :: Int64)
    , iso_min_max (10 :: Word)
    , iso_min_max (10 :: Word8)
    , iso_min_max (10 :: Word16)
    , iso_min_max (10 :: Word32)
    , iso_min_max (10 :: Word64)
    ]

fromIntDigits_eq :: Test
fromIntDigits_eq =
  "fromIntDigits_eq" ~:
    [ qc (undefined :: Int)     Prop.fromIntDigits_eq
    , qc (undefined :: Int8)    Prop.fromIntDigits_eq
    , qc (undefined :: Int16)   Prop.fromIntDigits_eq
    , qc (undefined :: Int32)   Prop.fromIntDigits_eq
    , qc (undefined :: Int64)   Prop.fromIntDigits_eq
    , qc (undefined :: Word)    Prop.fromIntDigits_eq
    , qc (undefined :: Word8)   Prop.fromIntDigits_eq
    , qc (undefined :: Word16)  Prop.fromIntDigits_eq
    , qc (undefined :: Word32)  Prop.fromIntDigits_eq
    , qc (undefined :: Word64)  Prop.fromIntDigits_eq
    , qc (undefined :: Integer) Prop.fromIntDigits_eq
    ]
  where
    qc :: (Show n, Data n, Arbitrary n, Testable prop) => n -> (n -> [n] -> prop) -> Test
    qc x p = dataTypeNameOf x ~|: p

toIntDigits_eq :: Test
toIntDigits_eq =
  "toIntDigits_eq" ~:
    [ qc (undefined :: Int)     Prop.toIntDigits_eq
    , qc (undefined :: Int8)    Prop.toIntDigits_eq
    , qc (undefined :: Int16)   Prop.toIntDigits_eq
    , qc (undefined :: Int32)   Prop.toIntDigits_eq
    , qc (undefined :: Int64)   Prop.toIntDigits_eq
    , qc (undefined :: Word)    Prop.toIntDigits_eq
    , qc (undefined :: Word8)   Prop.toIntDigits_eq
    , qc (undefined :: Word16)  Prop.toIntDigits_eq
    , qc (undefined :: Word32)  Prop.toIntDigits_eq
    , qc (undefined :: Word64)  Prop.toIntDigits_eq
    , qc (undefined :: Integer) Prop.toIntDigits_eq
    ]
  where
    qc :: (Show n, Data n, Arbitrary n, Testable prop) => n -> (n -> n -> prop) -> Test
    qc x p = dataTypeNameOf x ~|: p

toDigits_int_show :: Test
toDigits_int_show =
  "toDigits_int_show" ~:
    [ test_qc_typed1 (undefined :: Int)     Prop.toDigits_int_show
    , test_qc_typed1 (undefined :: Int8)    Prop.toDigits_int_show
    , test_qc_typed1 (undefined :: Int16)   Prop.toDigits_int_show
    , test_qc_typed1 (undefined :: Int32)   Prop.toDigits_int_show
    , test_qc_typed1 (undefined :: Int64)   Prop.toDigits_int_show
    , test_qc_typed1 (undefined :: Word)    Prop.toDigits_int_show
    , test_qc_typed1 (undefined :: Word8)   Prop.toDigits_int_show
    , test_qc_typed1 (undefined :: Word16)  Prop.toDigits_int_show
    , test_qc_typed1 (undefined :: Word32)  Prop.toDigits_int_show
    , test_qc_typed1 (undefined :: Word64)  Prop.toDigits_int_show
    , test_qc_typed1 (undefined :: Integer) Prop.toDigits_int_show
    ]

