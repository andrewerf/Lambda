module Core.AST
  (
  Name(..),
  Term(..),
  Context,
  emptyContext,
  getTerm,
  extendContext,
  extendContextGlobal,
  shift
  )
where

import Relude.List
 

-- Name of a variable
data Name
  = Local Int -- De Bruijn index of a local variable
  | Global String -- or name of a global variable
  deriving ( Eq, Show )


-- If lambda-C everything is a term, even types, as both types and terms could contain same intermixed constructs: applications and
-- abstractions. This unification is a key property of the system.
-- Therefore it maybe correct to write `t1 : t2` for some terms t1 and t2 (for instance, when t1 = Int, t2 = *).
-- In this case, we call `t2` a parent of `t1`.
-- Note that the word `term` is used interchangeably with the word `expression` here.
data Term
  = TmSq -- square (□). This is one level above kinds. `( Πx : *. * ) : □` (similar to `* -> * : □`). Also, `* : □` (sort-rule).
  | TmStar -- star (*). The basic kind.
  | TmVar Name -- bound or global variable (on term or type level)
  | TmAbs Term Term -- abstraction, `TmAbs t1 t2 <=> \x : t1. t2`
  | TmPi Term Term -- type of abstraction, `TmPi t1 t2 <=> Πx : t1. t2`, and if x is not used in t2, this is similar to `t1 -> t2`
  | TmApp Term Term -- application, `TmApp t1 t2 <=> t1 t2`
  deriving ( Eq )
  
instance Show Term where
  show TmSq = "□"
  show TmStar = "*"
  show ( TmVar ( Local x ) ) = show x
  show ( TmVar ( Global s ) ) = s
  show ( TmAbs a m ) = "\\:" ++ show a ++ "." ++ show m
  show ( TmPi a m ) = "Π:" ++ show a ++ "." ++ show m
  show ( TmApp t1 t2 ) = "(" ++ show t1 ++ " " ++ show t2 ++ ")" 


-- Context is a product of global and local contexts.
data Context = Context {
    globalContext :: [(String, Term)], -- contains global definitions, addressable by names
    localContext :: [Term] -- contains local definitions, addressable by indexes
  }
  deriving Show
  
emptyContext :: Context
emptyContext = Context [] []


getTerm :: Context -> Name -> Maybe Term
getTerm ctx ( Local i ) = shift ( i + 1 ) <$> localContext ctx !!? i
getTerm ctx ( Global s ) = lookup s ( globalContext ctx )


extendContext :: Context -> Term -> Context
extendContext ctx tm = Context ( globalContext ctx ) ( tm : localContext ctx )


extendContextGlobal :: Context -> String -> Term -> Context
extendContextGlobal ctx s tm = Context ( ( s, tm ) : globalContext ctx ) ( localContext ctx )


-- Shifts with a cut-off (first arg)
shift :: Int -> Term -> Term
shift = shift_ 0
  where
    shift_ k x tm@( TmVar ( Local t ) )
      | t < k = tm
      | otherwise = TmVar ( Local $ t + x )
    shift_ _ _ tm@( TmVar ( Global _ ) ) = tm
    shift_ _ _ TmSq = TmSq
    shift_ _ _ TmStar = TmStar
    shift_ k x ( TmAbs t1 t2 ) = TmAbs ( shift_ k x t1 ) ( shift_ (k + 1) x t2 )
    shift_ k x ( TmPi t1 t2 ) = TmPi ( shift_ k x t1 ) ( shift_ (k + 1) x t2 )
    shift_ k x ( TmApp t1 t2 ) = TmApp ( shift_ k x t1 ) ( shift_ k x t2 )

