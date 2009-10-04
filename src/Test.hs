{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-----------------------------------------------------------------------------

module Main () where

--------------------------------------------------------------------------------

import Test.HUnit ((~:), Test, runTestTT)

import qualified Test.EnumInt as EnumInt (tests)
import qualified Test.Count as Count (tests)

--------------------------------------------------------------------------------

main :: IO ()
main =
  do putStrLn "Running tests for digits..."
     runTestTT tests
     return ()

tests :: Test
tests =
  "Digits" ~:
    [ EnumInt.tests
    , Count.tests
    ]

