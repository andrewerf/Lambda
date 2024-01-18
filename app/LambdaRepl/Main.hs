module Main ( main ) where

import LambdaFrontend.REPL ( defaultReplState, repl )

main :: IO()
main = repl defaultReplState