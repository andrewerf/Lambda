{-# LANGUAGE TypeApplications #-}
module LambdaFrontend.REPL
  (
  Command(..),
  pep,
  processCommand,
  repl
  )
where

import System.IO

import LambdaFrontend.Parser ( parse )
import LambdaFrontend.AST

import qualified Core.Eval as E
import qualified Core.Typing as T

import Control.Exception
import Data.Void ( Void )

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
  
data Command
  = Quit
  | Help
  | PrintEnv
  | ExecuteFile String


type Parser = P.Parsec Void String

parsecCommand :: Parser Command
parsecCommand = P.choice [
    Quit <$ P.string ":quit",
    Help <$ P.string ":help",
    PrintEnv <$ P.string ":env",
    ExecuteFile <$> ( P.string ":exec " *> P.many P.anySingle )
  ]

parseCommand :: String -> Maybe Command
parseCommand = P.parseMaybe parsecCommand


read_ :: IO String
read_ = putStr "λ> "
     >> hFlush stdout
     >> getLine


eval :: Environment -> Term -> ( Environment, Maybe Term, Maybe Term )
eval env tm =
  let
    go :: Environment -> Term -> Environment
    go env ( TmLet s tl Nothing ) = ( s, tl ) : env
    go env ( TmLet s tl ( Just tll ) ) = ( s, tl ) : go env tll
    go env _ = env

    ctm = toCore env tm
    newEnv = go env tm
  in
    ( newEnv, fromCore . E.eval <$> ctm, fromCore <$> ( ctm >>= T.lift0 ) )


-- Parses and evaluates a term (given as a string). Returns modified environment
pep :: Environment -> String -> IO Environment
pep env inp = do
  let t = parse inp
  let ( newEnv, tm, tp ) = eval env t
  putStrLn ( show tm ++ " : " ++ show tp )
  return newEnv


-- Processes a single REPL command and returns modified environment
processCommand :: Environment -> Command -> IO Environment
processCommand env Quit = return env
processCommand env Help = do
  putStrLn "Usage: enter a lambda term to be evaluated. The output gives you the evaluation result and its type."
  putStrLn "Use special operator: `let x = s; let y = z ...` (without `in`) to add definition to the environment."
  putStrLn ":help -- print this help"
  putStrLn ":env -- print environment"
  putStrLn ":exec _file_ -- execute given file (by path). The file should contain a lambda term."
  return env
processCommand env PrintEnv = print env >> return env
processCommand env ( ExecuteFile fpath ) = do
  r <- ex2left @IOException ( readFile fpath )
  case r of
    Left e -> print e >> return env
    Right program -> do
      pep env program
  where
    ex2left :: Exception e => IO a -> IO ( Either e a )
    ex2left x = catch ( Right <$> x ) ( return . Left )


-- Launches the REPL which runs infinitely (until ":quit" is supplied)
repl :: Environment -> IO()
repl env = do
  inp <- read_
  case parseCommand inp of
    Nothing -> pep env inp >>= repl
    Just cmd -> processCommand env cmd >>= repl