module Core.AST
  (
  Binding(..),
  Term(..),
  getBindingName,
  shift,
  fvs
  )
where


-- This type represents different kinds of bindings. They have similar properties, so it is useful to abstract it.
data Binding
  = AbsBinding String -- term-level abstraction
  | PiBinding String -- type-level abstraction
  | LetBinding String -- let-binding

instance Eq Binding where
  (==) ( AbsBinding _ ) ( AbsBinding _ ) = True
  (==) ( PiBinding _ ) ( PiBinding _ ) = True
  (==) ( LetBinding _ ) ( LetBinding _ ) = True
  (==) _ _ = False

getBindingName :: Binding -> String
getBindingName ( AbsBinding s ) = s
getBindingName ( PiBinding s ) = s
getBindingName ( LetBinding s ) = s

-- If lambda-C everything is a term, even types, as both types and terms could contain same intermixed constructs: applications and
-- abstractions. This unification is a key property of the system.
-- Therefore it maybe correct to write `t1 : t2` for some terms t1 and t2 (for instance, when t1 = Int, t2 = *).
-- In this case, we call `t2` a parent of `t1`.
-- Note that the word `term` is used interchangeably with the word `expression` here.
data Term
  = TmSq -- square (□). This is one level above kinds. `( Πx : *. * ) : □` (similar to `* -> * : □`). Also, `* : □` (sort-rule).
  | TmStar -- star (*). The basic kind.
  | TmVar Int -- bound variable (on term or type level)
  | TmBind Binding Term Term -- let-binding `let x = t1 in t2`
  | TmApp Term Term -- application, `TmApp t1 t2 <=> t1 t2`
  deriving ( Eq )
  
instance Show Term where
  show TmSq = "□"
  show TmStar = "*"
  show ( TmVar x ) = show x
  show ( TmBind ( AbsBinding _ ) a m ) = "\\:" ++ show a ++ "." ++ show m
  show ( TmBind ( PiBinding _ ) a m ) = "Π:" ++ show a ++ "." ++ show m
  show ( TmBind ( LetBinding _ ) a m ) = "let = " ++ show a ++ " in " ++ show m
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
    shift_ k x ( TmBind bind t1 t2 ) = TmBind bind ( shift_ k x t1 ) ( shift_ (k + 1) x t2 )
    shift_ k x ( TmApp t1 t2 ) = TmApp ( shift_ k x t1 ) ( shift_ k x t2 )


-- Returns all free variables of the given term
fvs :: Term -> [Int]
fvs = fvs_ 0
  where
    -- first arg is a cut-off
    fvs_ :: Int -> Term -> [Int]
    fvs_ k ( TmVar t )
      | t < k = []
      | otherwise = [t - k]
    fvs_ _ TmSq = []
    fvs_ _ TmStar = []
    fvs_ k ( TmBind _ t1 t2 ) = fvs_ k t1 ++ fvs_ ( k + 1 ) t2
    fvs_ k ( TmApp t1 t2 ) = fvs_ k t1 ++ fvs_ k t2