module Main ( main ) where

import System.IO

import LambdaFrontend.Parser ( parse )
import LambdaFrontend.AST ( toCore, fromCore )

import Core.Eval ( eval )
import Core.Typing ( lift0 )

import Control.Monad ( unless )


-- REPL

read_ :: IO String
read_ = putStr "λ> "
     >> hFlush stdout
     >> getLine

eval_ :: String -> String
eval_ s = 
  let
    tm = toCore ( parse s )
    mtp = tm >>= lift0
  in
    show ( fromCore . eval <$> tm ) ++ " : " ++ show ( fromCore <$> mtp )

print_ :: String -> IO()
print_ = putStrLn

main :: IO()
main = do
  inp <- read_
  unless ( inp == ":quit" )
       $ print_ ( eval_ inp ) >> main