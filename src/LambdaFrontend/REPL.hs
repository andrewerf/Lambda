{-# LANGUAGE TypeApplications, QuasiQuotes #-}
module LambdaFrontend.REPL
  (
  Command(..),
  ReplState,
  defaultReplState,
  pep,
  processCommand,
  processString,
  repl
  )
where

import System.IO ( hFlush, stdout )

import Text.RawString.QQ

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
  | AscriptionMode Bool
  | Check String String


type Parser = P.Parsec Void String

parsecOnOff :: Parser Bool
parsecOnOff = ( True <$ P.string "on" ) P.<|> ( False <$ P.string "off" )

parsecCommand :: Parser Command
parsecCommand = P.choice [
    Quit <$ P.string ":quit",
    Help <$ P.string ":help",
    PrintEnv <$ P.string ":env",
    ExecuteFile <$> ( P.string ":exec " *> P.many P.anySingle ),
    AscriptionMode <$> ( P.string ":asc " *> parsecOnOff ),
    Check <$> ( P.string ":check \"" *> P.manyTill P.anySingle ( P.char '"' ) )
          <*> ( P.string " \"" *> P.manyTill P.anySingle ( P.char '"' ) )
  ]

parseCommand :: String -> Maybe Command
parseCommand = P.parseMaybe parsecCommand


read_ :: IO String
read_ = putStr "λ> "
     >> hFlush stdout
     >> getLine


data ReplState = ReplState {
    ascMode_ :: Bool,
    env_ :: Environment
  }

defaultReplState :: ReplState
defaultReplState = ReplState { ascMode_ = True, env_ = [] }


eval :: ReplState -> Term -> ( ReplState, Maybe Term, Maybe Term )
eval st tm =
  let
    env = env_ st
    ra = if ascMode_ st then replaceAscribed env else id

    go :: Environment -> Term -> Environment
    go genv ( TmLet s tl Nothing ) = ( s, tl ) : genv
    go genv ( TmLet s tl ( Just tll ) ) = ( s, tl ) : go genv tll
    go genv _ = genv

    ctm = toCore env tm
    newEnv = go env tm
    t1 = ra . fromCore . E.eval <$> ctm
    t2 = ra . fromCore <$> ( ctm >>= T.lift0 )
  in
    ( st{ env_ = newEnv }, t1, t2 )


-- Parses and evaluates a term (given as a string). Returns modified environment
pep :: ReplState -> String -> IO ( ReplState, String )
pep st inp = do
  let t = parse inp
  let ( newSt, tm, tp ) = eval st t
  return ( newSt, show tm ++ " : " ++ show tp )

helpMsg :: String
helpMsg = [r|This is a bare-bone lambda calculus interpreter, with dependent types.

The syntax is pretty simple:
  - Kind is denoted by `*`.
  - Sort rule is: `* : □`, which you can verify by typing `*` alone.
  - Abstraction is denoted with '\' and '.'. For example, this is the polymorphic identity function: `\X:*.\x:X.x`.
  - Abstraction on a type level (i.e. "forall" operator) is denoted by '@' and '.'. For example, this is the type of the polymorphic identity function: `@X:*.X -> X`.
  - Arrow `A -> B` is merely a syntactic construct upon Pi-type `@x:A.B` if x is not a free-variable in B (B doesn't depend on x).
  - There are two forms of `let` bindings:
    - First is the part of the core language and has the following form: `let x = A in T`. You can also use nested let-bindings.
    - Second is merely part of the repl and has the following form: `let x = A` (without `in`). This command modifies REPL environment, so that you can refer to `x` in it.
      For example, the following series of commands correctly calculates the value of logical expression `true & false` using Church-encoded booleans:
      λ> let Bool = @X:*.X->X->X;
      λ> let tru = \X:*.\x:X.\y:X.x;
      λ> let fls = \X:*.\x:X.\y:X.y;
      λ> let and = \a:Bool.\b:Bool.a Bool b fls;
      λ> and tru fls

Apart from lambda terms, the following commands are supported.
:help -- print this help
:env -- print environment
:asc [on|off] -- enter (leave) ascription mode. "on" by default.
 In this mode, all the terms are searched for the subterms, which are defined in the current environment, and replaced with their names.
 For example, if `let A = @X:*.*` is entered, and `asc` mode is turned on, then the printed type of the term `\X:*.X`
 would be `A`, in contrast to `@X:*.X` of `asc` mode is turned off.
:exec _file_ -- execute given file (by path). The file should contain a lambda term. Is not supported in the web-version.
:check "tm" "tp" -- checks that term `tm` has type `tp`. Prints True or False.
|]

-- Processes a single REPL command. Returns modified environment and response string
processCommand :: ReplState -> Command -> IO ( ReplState, String )
processCommand st Quit = return ( st, "" )
processCommand st Help = return ( st, helpMsg )
processCommand st PrintEnv = return ( st, show $ env_ st )
processCommand st ( ExecuteFile fpath ) = do
  rr <- ex2left @IOException ( readFile fpath )
  case rr of
    Left e -> return ( st, show e )
    Right program -> do
      pep st program
  where
    ex2left :: Exception e => IO a -> IO ( Either e a )
    ex2left x = catch ( Right <$> x ) ( return . Left )
processCommand st ( AscriptionMode asc ) = return ( st{ascMode_ = asc}, "" )
processCommand st ( Check s1 s2 ) =
  let
    ( _, _, lifted ) = eval st ( parse s1 )
    ( _, tp, _ ) = eval st ( parse s2 )
  in
    return ( st, show $ maybe False id ( liftA2 (==) tp lifted ) )


processString :: ReplState -> String -> IO ( ReplState, String )
processString st inp =
  case parseCommand inp of
    Nothing -> pep st inp
    Just cmd -> processCommand st cmd

-- Launches the REPL which runs infinitely (until ":quit" is supplied)
repl :: ReplState -> IO()
repl st = do
  inp <- read_
  ( newSt, resp ) <- processString st inp
  putStrLn resp
  repl newSt