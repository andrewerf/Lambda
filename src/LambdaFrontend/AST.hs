module LambdaFrontend.AST
  (
  Term(..),
  toCore,
  fromCore
  )
where

import Data.List ( elemIndex )
import qualified Core.AST as C

-- Same as in core, but with names instead of De Bruijn indices
data Term
  = TmSq
  | TmStar
  | TmVar String
  | TmAbs String Term Term
  | TmPi String Term Term
  | TmArrow Term Term -- the only "sugar" we provide for a lambda-calc: `A -> B` is the same as `Πx:A.B` if `x` not in `FV(B)`.
  | TmApp Term Term
  deriving Eq

instance Show Term where
  show TmSq = "□"
  show TmStar = "*"
  show ( TmVar s ) = s
  show ( TmAbs s a m ) = "\\" ++ s ++ ":" ++ show a ++ "." ++ show m
  show ( TmPi s a m ) = "Π" ++ s ++ ":" ++ show a ++ "." ++ show m
  show ( TmArrow a b ) = show a ++ " -> " ++ show b
  show ( TmApp t1 t2 ) = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"


-- Transforms to core AST
toCore :: Term -> C.Term
toCore = toCore_ []
  where
    toCore_ :: [String] -> Term -> C.Term
    toCore_ _ TmSq = C.TmSq
    toCore_ _ TmStar = C.TmStar
    toCore_ ctx ( TmVar s ) = case elemIndex s ctx of
      Just i -> C.TmVar ( C.Local i )
      Nothing -> C.TmVar ( C.Global s )
    toCore_ ctx ( TmAbs s t1 t2 ) = C.TmAbs ( toCore_ ctx t1 ) ( toCore_ ( s : ctx ) t2 )
    toCore_ ctx ( TmPi s t1 t2 ) = C.TmPi ( toCore_ ctx t1 ) ( toCore_ ( s : ctx ) t2 )
    toCore_ ctx ( TmArrow a b ) = toCore_ ctx ( TmPi "" a b )
    toCore_ ctx ( TmApp t1 t2 ) = C.TmApp ( toCore_ ctx t1 ) ( toCore_ ctx t2 )


-- Transforms a core term to the corresponding frontend term (introducing unique variable names instead of indexes)
fromCore :: C.Term -> Term
fromCore = fromCore_ []
  where
    fromCore_ :: [String] -> C.Term -> Term
    fromCore_ _ C.TmSq = TmSq
    fromCore_ _ C.TmStar = TmStar
    fromCore_ ctx ( C.TmVar ( C.Local i ) ) = TmVar $ ctx !! i
    fromCore_ _ ( C.TmVar ( C.Global s ) ) = TmVar s
    fromCore_ ctx ( C.TmAbs m n ) = TmAbs vn ( fromCore_ ctx m ) ( fromCore_ ( vn : ctx ) n )
      where vn = "x" ++ show ( length ctx )
    fromCore_ ctx ( C.TmPi m n ) = TmPi vn ( fromCore_ ctx m ) ( fromCore_ ( vn : ctx ) n )
      where vn = "x" ++ show ( length ctx )
    fromCore_ ctx ( C.TmApp m n ) = TmApp ( fromCore_ ctx m ) ( fromCore_ ctx n )
