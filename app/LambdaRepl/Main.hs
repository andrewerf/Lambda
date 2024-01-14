module Main ( main ) where

import System.IO

import LambdaFrontend.Parser ( parse )
import LambdaFrontend.AST

import qualified Core.Eval as E
import qualified Core.Typing as T


-- REPL

read_ :: IO String
read_ = putStr "λ> "
     >> hFlush stdout
     >> getLine

eval :: Environment -> Term -> ( Environment, Maybe Term, Maybe Term )
eval env tm =
  let
    ctm = toCore env tm
    newEnv = case tm of
      ( TmLet s tl ) -> ( s, tl ) : env
      _ -> env
  in
    ( newEnv, fromCore . E.eval <$> ctm, fromCore <$> ( ctm >>= T.lift0 ) )

repl :: Environment -> IO()
repl env = do
  inp <- read_
  case inp of
    ":quit" -> return ()
    ":env" -> print env >> repl env
    _ -> do
      let t = parse inp
      let ( newEnv, tm, tp ) = eval env t
      putStrLn ( show tm ++ " : " ++ show tp )
      repl newEnv

main :: IO()
main = repl []