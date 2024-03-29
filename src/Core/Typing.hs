module Core.Typing
  (
  Context,
  lift,
  lift0
  )
where

import Core.AST
import Core.Eval


import Relude.List


-- Type derivation context.
-- `Context !! i` represents the "type" (lift) of the i-th De Bruijn variable.
type Context = [Term]

-- Extracts term from the context. Automatically shifts it by the size of the context.
getTerm :: Context -> Int -> Maybe Term
getTerm ctx i = shift ( i + 1 ) <$> ctx !!? i


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
lift _ TmUnit = Just TmTUnit
lift _ TmTUnit = Just TmStar
lift ctx ( TmVar n ) = getTerm ctx n -- variable should be in context

lift ctx ( TmApp m n ) = do
  tf <- evalLazy <$> lift ctx m -- reduced type of function (conv + first part of app)
  a <- lift ctx n -- type of argument (second part of app)
  case tf of
    -- Π-type with matching type
    TmBind ( PiBinding _ ) a' b | betaEq a a' -> return $ substShift b n
    _ -> Nothing

lift ctx ( TmBind ( AbsBinding s ) a m ) = do
  let extCtx = a : ctx -- extended context
  b <- lift extCtx m -- type of tail (first part of abs)
  let r = TmBind ( PiBinding s ) a b
  _ <- lift ctx r -- type of abstraction (second part of abs)
  return r

lift ctx ( TmBind ( PiBinding _ ) a b ) = do
  _ <- lift ctx a -- s1 (first part of form)
  let extCtx = a : ctx -- extended context
  lift extCtx b -- s2 (second part of form)

lift ctx ( TmBind ( LetBinding _ ) a b ) = do
  ta <- lift ctx a
  let extCtx = ta : ctx -- extended context
  tb <- lift extCtx b
  return $ substShift tb a


-- A shortcut for a list with empty context
lift0 :: Term -> Maybe Term
lift0 = lift []