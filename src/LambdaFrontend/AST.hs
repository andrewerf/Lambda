module LambdaFrontend.AST
  (
  Term(..),
  Environment,
  toCore,
  fromCore,
  replaceAscribed
  )
where

import Data.List ( elemIndex )

import qualified Core.AST as C
import qualified Core.Eval as C

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
  | TmLet String Term ( Maybe Term ) -- special `let` for the REPL
  | TmUnit
  | TmTUnit
  deriving Eq

instance Show Term where
  show TmSq = "□"
  show TmStar = "*"
  show ( TmVar s ) = s
  show ( TmAbs s a m ) = "\\" ++ s ++ ":" ++ show a ++ "." ++ show m
  show ( TmPi s a m ) = "Π" ++ s ++ ":" ++ show a ++ "." ++ show m
  show ( TmArrow a b ) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show ( TmApp t1 t2 ) = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show ( TmLet x y Nothing ) = "let " ++ x ++ " = " ++ show y
  show ( TmLet x y ( Just tm ) ) = "let " ++ x ++ " = " ++ show y ++ "; " ++ show tm
  show ( TmLetIn x y t ) = "let " ++ x ++ " = " ++ show y ++ " in " ++ show t
  show TmUnit = "unit"
  show TmTUnit = "Unit"


type Environment = [(String, Term)]

-- Transforms to core AST
-- Takes `Environment` to lookup free variables (the variables for which the abstraction is not found)
toCore :: Environment -> Term -> Maybe C.Term
toCore = toCore_ []
  where
    toCore_ :: [String] -> Environment -> Term -> Maybe C.Term
    toCore_ _ _ TmSq = Just C.TmSq
    toCore_ _ _ TmStar = Just C.TmStar
    toCore_ _ _ TmUnit = Just C.TmUnit
    toCore_ _ _ TmTUnit = Just C.TmTUnit
    toCore_ ctx env ( TmVar s ) = case elemIndex s ctx of
      Nothing -> lookup s env >>= toCore_ ctx env
      Just i -> Just $ C.TmVar i
    toCore_ ctx env ( TmAbs s t1 t2 ) = liftA2 ( C.TmBind ( C.AbsBinding s ) ) ( toCore_ ctx env t1 ) ( toCore_ ( s : ctx ) env t2 )
    toCore_ ctx env ( TmPi s t1 t2 ) = liftA2 ( C.TmBind ( C.PiBinding s ) ) ( toCore_ ctx env t1 ) ( toCore_ ( s : ctx ) env t2 )
    toCore_ ctx env ( TmLetIn s t1 t2 ) = liftA2 ( C.TmBind ( C.LetBinding s ) ) ( toCore_ ctx env t1 ) ( toCore_ ( s : ctx ) env t2 )
    toCore_ ctx env ( TmArrow a b ) = toCore_ ctx env ( TmPi "" a b )
    toCore_ ctx env ( TmApp t1 t2 ) = liftA2 C.TmApp ( toCore_ ctx env t1 ) ( toCore_ ctx env t2 )
    toCore_ ctx env ( TmLet _ tm Nothing ) = toCore_ ctx env tm
    toCore_ ctx env ( TmLet s tm ( Just mtm ) ) = toCore_ ctx ( ( s, tm ) : env ) mtm

-- Transforms a core term to the corresponding frontend term (introducing unique variable names instead of indexes)
fromCore :: C.Term -> Term
fromCore = fromCore_ []
  where
    fromCore_ :: [String] -> C.Term -> Term
    fromCore_ _ C.TmSq = TmSq
    fromCore_ _ C.TmStar = TmStar
    fromCore_ _ C.TmUnit = TmUnit
    fromCore_ _ C.TmTUnit = TmTUnit
    fromCore_ ctx ( C.TmVar i ) = TmVar $ ctx !! i
    fromCore_ ctx ( C.TmBind binding m n ) = case binding of
      ( C.PiBinding _ ) | 0 `notElem` C.fvs n -> TmArrow ( fctx m ) ( fctx $ C.shift (-1) n )
      ( C.PiBinding _ ) -> TmPi vn ( fctx m ) ( fctxExt n )
      ( C.LetBinding _ ) -> TmLetIn vn ( fctx m ) ( fctxExt n )
      ( C.AbsBinding _ ) -> TmAbs vn ( fctx m ) ( fctxExt n )
      where
        vn_ = C.getBindingName binding
        vn = if length vn_ == 0 then "x" ++ show ( length ctx ) else vn_
        fctx = fromCore_ ctx
        fctxExt = fromCore_ ( vn : ctx )
    fromCore_ ctx ( C.TmApp m n ) = TmApp ( fromCore_ ctx m ) ( fromCore_ ctx n )


-- Replaces all the subterms that have a name in the given environment
replaceAscribed :: Environment -> Term -> Term
replaceAscribed env tm = case filter ( pr tm . snd ) env of
  ( hd : _ ) -> TmVar ( fst hd )
  [] -> case tm of
    TmSq -> TmSq
    TmStar -> TmStar
    TmUnit -> TmUnit
    TmTUnit -> TmTUnit
    TmVar s -> TmVar s
    TmAbs s a b -> TmAbs s ( r a ) ( r b )
    TmPi s a b -> TmPi s ( r a ) ( r b )
    TmArrow a b -> TmArrow ( r a ) ( r b )
    TmApp a b -> TmApp ( r a ) ( r b )
    t@TmLetIn{} -> t
    TmLet s tm mtm -> TmLet s ( r tm ) ( r <$> mtm )
  where
    r = replaceAscribed env

    pr :: Term -> Term -> Bool
    pr t1 t2 = maybe False id $ do
      tc1 <- toCore env t1
      tc2 <- toCore env t2
      return $ C.eval tc1 == C.eval tc2