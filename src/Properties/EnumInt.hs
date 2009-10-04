{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Properties.EnumInt
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-----------------------------------------------------------------------------

module Properties.EnumInt where

--------------------------------------------------------------------------------

import Data.Char (ord)

import Digits.EnumInt

--------------------------------------------------------------------------------

iso :: (EnumIntDigits n) => n -> n -> Bool
iso base number = r1 == r2
  where
    r1 = toIntDigits base number
    r2 = r1 >>= toIntDigits base . fromIntDigits

fromIntDigits_eq :: (EnumIntDigits n) => n -> [n] -> Bool
fromIntDigits_eq base digits
  | base < 2  = result == Nothing
  | invalid   = result == Nothing
  | otherwise = result == Just number
  where
    result = fromIntDigits' base digits
    illegal n = n < 0 || n >= base
    invalid = null digits || any illegal digits
    number = to base $ reverse digits

toIntDigits_eq :: (EnumIntDigits n) => n -> n -> Bool
toIntDigits_eq base number
  | base < 2  = result == Nothing
  | otherwise = result == Just digits
  where
    result = toIntDigits base number >>= return . digitsOf 
    digits = reverse $ from base number

to :: (Num n) => n -> [n] -> n
to _ []       = 0
to b (d':ds') = go b d' ds'
  where
    go _ r []     = r
    go n r (d:ds) = go (n * b) (d * n + r) ds

from :: (Integral n) => n -> n -> [n]
from _ 0 = [0]
from b n
  | n < 0 = negate (n `rem` b) : go (negate (n `quot` b))
  | otherwise = go n
  where
    go 0 = []
    go r = r `rem` b : go (r `quot` b)

resolveDigits :: (Integral n) => n -> [n] -> [n]
resolveDigits base = reverse . from base . to base . reverse

toDigits_int_show :: (EnumIntDigits n) => n -> Bool
toDigits_int_show number = left == right
  where
    left = toDecimalDigits number
    str = show (abs (toInteger number))
    toDigit c = fromIntegral (ord c - ord '0')
    right = map toDigit $ takeWhile (/= '.') str

