import Test.HUnit
import Test.Hspec
import Test.Hspec.Contrib.HUnit

import Test.Core.AST
import Test.Core.Eval
import Test.Core.Typing

import Test.LambdaFrontend.Parser

tests :: Test
tests = TestList [
    TestLabel "Core" $ TestList [
      TestLabel "AST" testsAST,
      TestLabel "Eval" testsEval,
      TestLabel "Typing" testsTyping
    ],
    TestLabel "LambdaFrontend" $ TestList [
      TestLabel "Parser" testsParser
    ]
  ]

main :: IO ()
main = hspec $ fromHUnitTest tests
