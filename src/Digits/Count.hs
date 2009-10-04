{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Properties.Enum
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-----------------------------------------------------------------------------

module Digits.Count (
  CountDigits(..),
  countDecimalDigits,
  countIntegralDigits,
  countFractionalDigits,
) where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word

--------------------------------------------------------------------------------

class CountDigits n where

  countDigits :: n -> n -> (Word, Word)
  countDigits base number = (countDigitsL base number, countDigitsR base number)

  countDigitsL :: n -> n -> Word
  countDigitsL base number = fst (countDigits base number)

  countDigitsR :: n -> n -> Word
  countDigitsL base number = snd (countDigits base number)

--------------------------------------------------------------------------------

countDecimalDigits :: (CountDigits n, Num n) => n -> (Word, Word)
countDecimalDigits = countDigits 10

countIntegralDigits :: (CountDigits n, Num n) => n -> Word
countIntegralDigits = countDigitsL 10

countFractionalDigits :: (CountDigits n, Num n) => n -> Word
countFractionalDigits = countDigitsR 10

--------------------------------------------------------------------------------

instance CountDigits Int where
  countDigitsL = int
  countDigitsR _ _ = 0

instance CountDigits Int8 where
  countDigitsL = int
  countDigitsR _ _ = 0

instance CountDigits Int16 where
  countDigitsL = int
  countDigitsR _ _ = 0

instance CountDigits Int32 where
  countDigitsL = int
  countDigitsR _ _ = 0

instance CountDigits Int64 where
  countDigitsL = int
  countDigitsR _ _ = 0

--------------------------------------------------------------------------------

instance CountDigits Word where
  countDigitsL = int
  countDigitsR _ _ = 0

instance CountDigits Word8 where
  countDigitsL = int
  countDigitsR _ _ = 0

instance CountDigits Word16 where
  countDigitsL = int
  countDigitsR _ _ = 0

instance CountDigits Word32 where
  countDigitsL = int
  countDigitsR _ _ = 0

instance CountDigits Word64 where
  countDigitsL = int
  countDigitsR _ _ = 0

--------------------------------------------------------------------------------

instance CountDigits Integer where
  countDigitsL = integer
  countDigitsR _ _ = 0

--------------------------------------------------------------------------------

-- | Count the number of digits in an non-negative integral.

intNonNeg :: (Integral a) => a -> a -> Word
intNonNeg !base = go 1
  where
    go !c !number
      | number < base = c
      | otherwise     = go (1 + c) (quot number base)

-- | Count the number of digits in an integral.

int :: (Integral a, Bounded a) => a -> a -> Word
int !base !number
  | number < 0 = 
    if number == minBound then
      1 + intNonNeg base (negate (quot number base))
    else
      intNonNeg base (negate number)
  | otherwise =
    intNonNeg base number

-- | Count the number of digits in an Integer.

integer :: (Integral a) => a -> a -> Word
integer !base !number
  | number < 0 = intNonNeg base (negate number)
  | otherwise  = intNonNeg base number

