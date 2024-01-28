{-# LANGUAGE DeriveFoldable #-}

module Core.AST
  (
  FunctorWithDepth(..),
  Binding(..),
  PTerm(..),
  Term,
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
data PTerm a
  = TmSq -- square (□). This is one level above kinds. `( Πx : *. * ) : □` (similar to `* -> * : □`). Also, `* : □` (sort-rule).
  | TmStar -- star (*). The basic kind.
  | TmVar a -- bound variable (on term or type level)
  | TmBind Binding ( PTerm a ) ( PTerm a ) -- let-binding `let x = t1 in t2`
  | TmApp ( PTerm a ) ( PTerm a ) -- application, `TmApp t1 t2 <=> t1 t2`
  | TmUnit -- unit term
  | TmTUnit -- unit type (with the only inhabitant: unit term above)
  deriving ( Eq, Foldable )

type Term = PTerm Int

instance Show a => Show ( PTerm a ) where
  show TmSq = "□"
  show TmStar = "*"
  show ( TmVar x ) = show x
  show ( TmBind ( AbsBinding _ ) a m ) = "\\:" ++ show a ++ "." ++ show m
  show ( TmBind ( PiBinding _ ) a m ) = "Π:" ++ show a ++ "." ++ show m
  show ( TmBind ( LetBinding _ ) a m ) = "let = " ++ show a ++ " in " ++ show m
  show ( TmApp t1 t2 ) = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show TmUnit = "unit"
  show TmTUnit = "Unit"


class FunctorWithDepth f where
  fmapWithDepth :: ( Int -> a -> b ) -> f a -> f b

instance FunctorWithDepth PTerm where
  fmapWithDepth = go 0
    where
      go :: Int -> ( Int -> a -> b ) -> PTerm a -> PTerm b
      go _ _ TmSq = TmSq
      go _ _ TmStar = TmStar
      go i f ( TmVar x ) = TmVar $ f i x
      go i f ( TmBind b t1 t2 ) = TmBind b ( go i f t1 ) ( go ( i + 1 ) f t2 )
      go i f ( TmApp t1 t2 ) = TmApp ( go i f t1 ) ( go i f t2 )
      go _ _ TmUnit = TmUnit
      go _ _ TmTUnit = TmTUnit

instance Functor PTerm where
  fmap f = fmapWithDepth ( const f )


-- Shifts with a cut-off (first arg)
shift :: Int -> Term -> Term
shift s = fmapWithDepth f
  where
    f :: Int -> Int -> Int
    f dep var
      | var < dep = var
      | otherwise = s + var

-- Returns all free variables of the given term
fvs :: Term -> [Int]
fvs = concat . fmapWithDepth f
  where
    f :: Int -> Int -> [Int]
    f dep var
      | var < dep = []
      | otherwise = [var - dep]
