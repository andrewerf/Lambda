import Test.HUnit
import Test.Hspec
import Test.Hspec.Contrib.HUnit

import Test.Core.Eval
import Test.Core.Typing

tests :: Test
tests = TestList [
    TestLabel "Core" $ TestList [
      TestLabel "Eval" testsEval,
      TestLabel "Typing" testsTyping
    ]
  ]

main :: IO ()
main = hspec $ fromHUnitTest tests
