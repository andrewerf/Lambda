module Core.Eval
  (
  subst,
  subst0,
  substShift,
  evalLazy,
  eval
  )
where

import Core.AST


-- `subst i t x` is t[i = x].
subst :: Int -> Term -> Term -> Term
subst _ TmSq _ = TmSq
subst _ TmStar _ = TmStar
subst i t@( TmVar j ) x
  | i == j = x
  | otherwise = t
subst i ( TmBind bind a b ) x = TmBind bind ( subst i a x ) ( subst ( i + 1 ) b ( shift 1 x ) )
subst i ( TmApp a b ) x = TmApp ( subst i a x ) ( subst i b x )


-- `subst0 t x` is t[0 = x].
subst0 :: Term -> Term -> Term
subst0 = subst 0


-- subst and shift (used for beta-reduction)
substShift :: Term -> Term -> Term
substShift t x = shift (-1) $ subst0 t ( shift 1 x )


-- Evaluates term lazily (only outermost applications)
evalLazy :: Term -> Term
evalLazy = go []
  where
    go :: [Term] -> Term -> Term
    go l ( TmApp f x ) = go (x : l) f
    go l ( TmBind ( LetBinding _ ) x y ) = go l ( substShift y x )
    go (x : l) ( TmBind ( AbsBinding _ ) _ t ) = go l ( substShift t x )
    go (x : l) ( TmBind ( PiBinding _ ) _ t ) = go l ( substShift t x )
    go l f = foldl TmApp f l

-- Evaluates given term with all sub expressions
eval :: Term -> Term
eval = go []
  where
    go :: [Term] -> Term -> Term
    go l ( TmApp f x ) = go (x : l) f
    go l ( TmBind ( LetBinding _ ) x y ) = go l ( substShift y x )
    go [] ( TmBind ( AbsBinding s ) a t ) = TmBind ( AbsBinding s ) ( eval a ) ( eval t )
    go (x : l) ( TmBind ( AbsBinding _ ) _ t ) = go l ( substShift t x )
    go [] ( TmBind ( PiBinding s ) a t ) = TmBind ( PiBinding s ) ( eval a ) ( eval t)
    go (x : l) ( TmBind ( PiBinding _ ) _ t ) = go l ( substShift t x )
    go l f = foldl TmApp f ( map eval l )
