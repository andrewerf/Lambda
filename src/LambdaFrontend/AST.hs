module LambdaFrontend.AST
  (
  Term(..),
  Environment,
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
  | TmArrow Term Term -- "sugar": `A -> B` is the same as `Πx:A.B` if `x` not in `FV(B)`.
  | TmApp Term Term
  | TmLetIn String Term Term
  | TmLet String Term -- special `let` for the REPL
  deriving Eq

instance Show Term where
  show TmSq = "□"
  show TmStar = "*"
  show ( TmVar s ) = s
  show ( TmAbs s a m ) = "\\" ++ s ++ ":" ++ show a ++ "." ++ show m
  show ( TmPi s a m ) = "Π" ++ s ++ ":" ++ show a ++ "." ++ show m
  show ( TmArrow a b ) = show a ++ " -> " ++ show b
  show ( TmApp t1 t2 ) = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show ( TmLet x y ) = "let " ++ x ++ " = " ++ show y
  show ( TmLetIn x y t ) = "let " ++ x ++ " = " ++ show y ++ " in " ++ show t


type Environment = [(String, Term)]

-- Transforms to core AST
-- Takes `Environment` to lookup free variables (the variables for which the abstraction is not found)
toCore :: Environment -> Term -> Maybe C.Term
toCore = toCore_ []
  where
    toCore_ :: [String] -> Environment -> Term -> Maybe C.Term
    toCore_ _ _ TmSq = Just C.TmSq
    toCore_ _ _ TmStar = Just C.TmStar
    toCore_ ctx env ( TmVar s ) = case elemIndex s ctx of
      Nothing -> lookup s env >>= toCore_ ctx env
      Just i -> Just $ C.TmVar i
    toCore_ ctx env ( TmAbs s t1 t2 ) = liftA2 ( C.TmBind C.AbsBinding ) ( toCore_ ctx env t1 ) ( toCore_ ( s : ctx ) env t2 )
    toCore_ ctx env ( TmPi s t1 t2 ) = liftA2 ( C.TmBind C.PiBinding ) ( toCore_ ctx env t1 ) ( toCore_ ( s : ctx ) env t2 )
    toCore_ ctx env ( TmLetIn s t1 t2 ) = liftA2 ( C.TmBind C.LetBinding ) ( toCore_ ctx env t1 ) ( toCore_ ( s : ctx ) env t2 )
    toCore_ ctx env ( TmArrow a b ) = toCore_ ctx env ( TmPi "" a b )
    toCore_ ctx env ( TmApp t1 t2 ) = liftA2 C.TmApp ( toCore_ ctx env t1 ) ( toCore_ ctx env t2 )
    toCore_ ctx env ( TmLet _ tm ) = toCore_ ctx env tm

-- Transforms a core term to the corresponding frontend term (introducing unique variable names instead of indexes)
fromCore :: C.Term -> Term
fromCore = fromCore_ []
  where
    fromCore_ :: [String] -> C.Term -> Term
    fromCore_ _ C.TmSq = TmSq
    fromCore_ _ C.TmStar = TmStar
    fromCore_ ctx ( C.TmVar i ) = TmVar $ ctx !! i
    fromCore_ ctx ( C.TmBind binding m n ) =
      let
        f :: C.Binding -> String -> Term -> Term -> Term
        f C.AbsBinding = TmAbs
        f C.PiBinding = TmPi
        f C.LetBinding = TmLetIn
      in
        f binding vn ( fromCore_ ctx m ) ( fromCore_ ( vn : ctx ) n )
      where vn = "x" ++ show ( length ctx )
    fromCore_ ctx ( C.TmApp m n ) = TmApp ( fromCore_ ctx m ) ( fromCore_ ctx n )
