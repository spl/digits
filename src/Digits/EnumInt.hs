{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Digits.EnumInt
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-----------------------------------------------------------------------------

module Digits.EnumInt (
  IntDigits,
  baseOf,
  digitsOf,
  EnumIntDigits(..),
  fromIntDigits',
  toDecimalDigits,
  fromDecimalDigits,
) where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Int
import Data.Word

import Data.Maybe (fromJust)

--------------------------------------------------------------------------------

data IntDigits n = IntDigits { baseOf :: n, digitsOf :: [n] }
  deriving Eq

instance (Show n) => Show (IntDigits n) where
  showsPrec p (IntDigits base digits)
    = showParen (p >= 11)
    $ showString "IntDigits "
    . showsPrec 0 base
    . showString " "
    . showsPrec 0 digits

--------------------------------------------------------------------------------

-- | The class 'EnumIntDigits' has methods for converting between a number and a
-- list of the number's digits in a given base representation.

class (Integral n) => EnumIntDigits n where

  toIntDigits :: n -> n -> Maybe (IntDigits n)

  fromIntDigits :: IntDigits n -> n
  fromIntDigits (IntDigits base digits) = go 0 digits
    where
      go !n []       = n
      go !n (d : ds) = go (base * n + d) ds

--------------------------------------------------------------------------------

fromIntDigits' :: (EnumIntDigits n) => n -> [n] -> Maybe n
fromIntDigits' base digits
  | base < 2                              = Nothing
  | null digits                           = Nothing
  | any (\d -> d < 0 || d >= base) digits = Nothing
  | otherwise                             = Just (fromIntDigits (IntDigits base digits))

toDecimalDigits :: (EnumIntDigits n) => n -> [n]
toDecimalDigits number = fromJust $ toIntDigits 10 number >>= return . digitsOf

fromDecimalDigits :: (EnumIntDigits n) => [n] -> Maybe n
fromDecimalDigits digits = fromIntDigits' 10 digits

--------------------------------------------------------------------------------
-- Instances: Int
--------------------------------------------------------------------------------

instance EnumIntDigits Int where
  toIntDigits = fromBounded

instance EnumIntDigits Int8 where
  toIntDigits = fromBounded

instance EnumIntDigits Int16 where
  toIntDigits = fromBounded

instance EnumIntDigits Int32 where
  toIntDigits = fromBounded

instance EnumIntDigits Int64 where
  toIntDigits = fromBounded

--------------------------------------------------------------------------------
-- Instances: Word
--------------------------------------------------------------------------------

instance EnumIntDigits Word where
  toIntDigits = fromBounded

instance EnumIntDigits Word8 where
  toIntDigits = fromBounded

instance EnumIntDigits Word16 where
  toIntDigits = fromBounded

instance EnumIntDigits Word32 where
  toIntDigits = fromBounded

instance EnumIntDigits Word64 where
  toIntDigits = fromBounded

--------------------------------------------------------------------------------
-- Instances: Integer
--------------------------------------------------------------------------------

instance EnumIntDigits Integer where
  toIntDigits = fromUnbounded

--------------------------------------------------------------------------------
-- EnumIntDigits: to: integral -> list of integral digits
--------------------------------------------------------------------------------

-- | Convert a non-negatve integral to a list of integral digits in an arbitrary
-- base.

intArbNonNegToListInt :: (Integral n) => n -> n -> [n] -> [n]
intArbNonNegToListInt !base = go
  where
    go !n ns
      | n < base  = n : ns
      | otherwise = go (quot n base) (rem n base : ns)

-- | Convert a non-negative integral to a list of integral digits in a
-- power-of-2 base. Uses base, shift, and mask.

intBinNonNegToListInt :: (Ord n, Bits n) => n -> Int -> n -> n -> [n] -> [n]
intBinNonNegToListInt !b !s !m = go
  where
    go !n ns
      | n < b = n : ns
      | otherwise = go (n `shiftR` s) (n .&. m : ns)

-- | Convert an integral to a list of integral digits in an arbitrary base.

intArbToListInt :: (Integral n, Bounded n) => n -> n -> [n]
intArbToListInt !base !number
  | number < 0 = 
    if number == minBound then
      let
        n = negate (quot number base)
        ns = intArbNonNegToListInt base (negate (rem number base)) []
      in
        intArbNonNegToListInt base n ns
    else
      intArbNonNegToListInt base (negate number) []
  | otherwise = intArbNonNegToListInt base number []

-- | Convert an integral to a list of integral digits in a power-of-2 base.

intBinToListInt :: (Bits n, Ord n, Bounded n) => n -> Int -> n -> n -> [n]
intBinToListInt !base !shft !mask !number
  | number < 0 =
    if number == minBound then
      let
        n = negate (number `shiftR` shft)
        ns = [number .&. mask]
      in
        intBinNonNegToListInt base shft mask n ns
    else
      intBinNonNegToListInt base shft mask (negate number) []
  | otherwise = intBinNonNegToListInt base shft mask number []

-- | Convert a bounded integral to a list of integral digits in an arbitrary
-- base.

fromBounded :: (Integral n, Bits n, Bounded n) => n -> n -> Maybe (IntDigits n)
fromBounded !base number
  | base < 2  = Nothing
  | otherwise =
    case base of
      2    -> wrap $ intBinToListInt 2    1  1    number
      4    -> wrap $ intBinToListInt 4    2  3    number
      8    -> wrap $ intBinToListInt 8    3  7    number
      16   -> wrap $ intBinToListInt 16   4  15   number
      32   -> wrap $ intBinToListInt 32   5  31   number
      64   -> wrap $ intBinToListInt 64   6  63   number
      128  -> wrap $ intBinToListInt 128  7  127  number
      256  -> wrap $ intBinToListInt 256  8  255  number
      512  -> wrap $ intBinToListInt 512  9  511  number
      _    -> wrap $ intArbToListInt base number
  where
    wrap = Just . IntDigits base

-- | Convert an unbounded integral to a list of Integer digits in an arbitrary
-- base.

fromUnbounded :: (Integral n) => n -> n -> Maybe (IntDigits n)
fromUnbounded !base number
  | base < 2   = Nothing
  | number < 0 = wrap $ intArbNonNegToListInt base (negate number) []
  | otherwise  = wrap $ intArbNonNegToListInt base number          []
  where
    wrap = Just . IntDigits base

