{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Properties.Count
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-----------------------------------------------------------------------------

module Properties.Count where

--------------------------------------------------------------------------------

import Digits.Count (CountDigits(..), countIntegralDigits)

--------------------------------------------------------------------------------

countDigits_int_show :: (CountDigits n, Integral n) => n -> Bool
countDigits_int_show number = left == right
  where
    left  = countIntegralDigits number
    str   = show (abs (toInteger number))
    right = fromIntegral $ length $ takeWhile (/= '.') str

