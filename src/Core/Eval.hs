module Core.Eval where

import Core.AST


-- `subst i t x` is t[i = x].
subst :: Int -> Term -> Term -> Term
subst _ TmSq _ = TmSq
subst _ TmStar _ = TmStar
subst i t@( TmVar ( Local j ) ) x
  | i == j = x
  | otherwise = t
subst _ t@( TmVar _ ) _ = t
subst i ( TmAbs a b ) x = TmAbs ( subst i a x ) ( subst ( i + 1 ) b ( shift0 1 x ) )
subst i ( TmApp a b ) x = TmApp ( subst i a x ) ( subst i b x )
subst i ( TmPi a b ) x = TmPi ( subst i a x ) ( subst ( i + 1 ) b ( shift0 1 x ) )


-- `subst0 t x` is t[0 = x].
subst0 :: Term -> Term -> Term
subst0 = subst 0


-- subst and shift (used for beta-reduction)
substShift :: Term -> Term -> Term
substShift t x = shift0 (-1) $ subst0 t ( shift0 1 x )


-- Evaluates term lazily (only outermost applications)
evalLazy :: Term -> Term
evalLazy = go []
  where
    go :: [Term] -> Term -> Term
    go l ( TmApp f x ) = go (x : l) f
    go (x : l) ( TmAbs _ t ) = go l ( substShift t x )
    go l f = foldl TmApp f l

-- Evaluates given term with all sub expressions
eval :: Term -> Term
eval = go []
  where
    go :: [Term] -> Term -> Term
    go l ( TmApp f x ) = go (x : l) f
    go [] ( TmAbs a t ) = TmAbs a ( eval t )
    go (x : l) ( TmAbs _ t ) = go l ( substShift t x )
    go l f = foldl TmApp f ( map eval l )
