{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Core.AST
  (
  testsAST
  )
where

import Test.Core.Numerals

import Test.HUnit
import Helper

import Core.AST


testFvs = TestLabel "Free Variables" $ numberedTestList [
    TestCase $ assertEqual "FV(\\:*.0) == {}" [] $ fvs ( mkAbs TmStar loc0 ),
    TestCase $ assertEqual "FV(\\:*.1 X) == {0}" [0] $ fvs ( mkAbs TmStar ( mkApp2 loc1 loc0 ) ),
    TestCase $ assertEqual "FV\\:*. ( \\:0.0 ) ( \\:0.1 ) == {}" [] $ fvs ( mkAbs TmStar $ mkApp2 ( mkAbs loc0 loc0 ) ( mkAbs loc0 loc1 ) ),
    TestCase $ assertEqual "FV\\:*. ( \\:0.0 ) ( \\:0.2 ) == {0}" [0] $ fvs ( mkAbs TmStar $ mkApp2 ( mkAbs loc0 loc0 ) ( mkAbs loc0 loc2 ) ),
    TestCase $ assertEqual "FV\\:*. ( \\:0.3 ) ( \\:0.2 ) == {1, 0}" [1, 0] $ fvs ( mkAbs TmStar $ mkApp2 ( mkAbs loc0 loc3 ) ( mkAbs loc0 loc2 ) )
  ]

testsAST = TestList [
    testFvs
  ]