module Core.Typing
  (
  lift,
  lift0
  )
where

import Core.AST
import Core.Eval


-- Checks if two terms are beta-equivalent (meaning they have similar normal forms)
betaEq :: Term -> Term -> Bool
betaEq t1 t2 = eval t1 == eval t2


-- Takes a term (in a context), and lifts it one level up, or returns nothing.
-- By one level up, we mean:
-- term -> type
-- type -> kind
-- kind -> □
lift :: Context -> Term -> Maybe Term
lift _ TmSq = Nothing
lift _ TmStar = Just TmSq
lift ctx ( TmVar n ) = getTerm ctx n -- variable should be in context

lift ctx ( TmApp m n ) = do
  tf <- evalLazy <$> lift ctx m -- reduced type of function (conv + first part of app)
  a <- lift ctx n -- type of argument (second part of app)
  case tf of
    -- Π-type with matching type
    TmPi a' b | betaEq a a' -> return $ substShift b n
    _ -> Nothing

lift ctx ( TmAbs a m ) = do
  let extCtx = extendContext ctx a -- extended context
  b <- lift extCtx m -- type of tail (first part of abs)
  let r = TmPi a b
  _ <- lift ctx r -- type of abstraction (second part of abs)
  return r

lift ctx ( TmPi a b ) = do
  _ <- lift ctx a -- s1 (first part of form)
  let extCtx = extendContext ctx a -- extended context
  lift extCtx b -- s2 (second part of form)


-- A shortcut for a list with empty context
lift0 :: Term -> Maybe Term
lift0 = lift emptyContext