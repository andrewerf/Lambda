module Core.AST
  (
  Term(..),
  shift
  )
where

-- If lambda-C everything is a term, even types, as both types and terms could contain same intermixed constructs: applications and
-- abstractions. This unification is a key property of the system.
-- Therefore it maybe correct to write `t1 : t2` for some terms t1 and t2 (for instance, when t1 = Int, t2 = *).
-- In this case, we call `t2` a parent of `t1`.
-- Note that the word `term` is used interchangeably with the word `expression` here.
data Term
  = TmSq -- square (□). This is one level above kinds. `( Πx : *. * ) : □` (similar to `* -> * : □`). Also, `* : □` (sort-rule).
  | TmStar -- star (*). The basic kind.
  | TmVar Int -- bound variable (on term or type level)
  | TmAbs Term Term -- abstraction, `TmAbs t1 t2 <=> \x : t1. t2`
  | TmPi Term Term -- type of abstraction, `TmPi t1 t2 <=> Πx : t1. t2`, and if x is not used in t2, this is similar to `t1 -> t2`
  | TmApp Term Term -- application, `TmApp t1 t2 <=> t1 t2`
  deriving ( Eq )
  
instance Show Term where
  show TmSq = "□"
  show TmStar = "*"
  show ( TmVar x ) = show x
  show ( TmAbs a m ) = "\\:" ++ show a ++ "." ++ show m
  show ( TmPi a m ) = "Π:" ++ show a ++ "." ++ show m
  show ( TmApp t1 t2 ) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

-- Shifts with a cut-off (first arg)
shift :: Int -> Term -> Term
shift = shift_ 0
  where
    shift_ k x tm@( TmVar t )
      | t < k = tm
      | otherwise = TmVar ( t + x )
    shift_ _ _ TmSq = TmSq
    shift_ _ _ TmStar = TmStar
    shift_ k x ( TmAbs t1 t2 ) = TmAbs ( shift_ k x t1 ) ( shift_ (k + 1) x t2 )
    shift_ k x ( TmPi t1 t2 ) = TmPi ( shift_ k x t1 ) ( shift_ (k + 1) x t2 )
    shift_ k x ( TmApp t1 t2 ) = TmApp ( shift_ k x t1 ) ( shift_ k x t2 )

