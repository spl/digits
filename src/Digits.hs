{-# OPTIONS_GHC -Wall #-}

module Digits (
  Digits(..),
  EnumDigits(..),
  toDecimalDigits,
  toIntegralDigits,
  toFractionalDigits,
  fromDecimalDigits,
  fromIntegralDigits,
  fromFractionalDigits,
  CountDigits(..),
  countDecimalDigits,
  countIntegralDigits,
  countFractionalDigits,
) where

--------------------------------------------------------------------------------

import Digits.Count
import Digits.Enum

--------------------------------------------------------------------------------

{-
Options for digits:
* [] Char
* Bytestring
* Array Int, ... ?
* Float/Double doesn't make sense, though it may not be difficult
-}

