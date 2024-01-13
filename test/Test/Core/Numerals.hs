{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Core.Numerals where


import Core.AST


mkLocalVar :: Int -> Term
mkLocalVar = TmVar . Local

loc0 = mkLocalVar 0
loc1 = mkLocalVar 1
loc2 = mkLocalVar 2
loc3 = mkLocalVar 3
loc4 = mkLocalVar 4


mkApp2 :: Term -> Term -> Term
mkApp2 = TmApp

mkApp3 :: Term -> Term -> Term -> Term
mkApp3 t1 t2 = mkApp2 ( mkApp2 t1 t2 )

mkApp4 :: Term -> Term -> Term -> Term -> Term
mkApp4 t1 t2 t3 = mkApp2 ( mkApp3 t1 t2 t3 )


mkArrow2 :: Term -> Term -> Term
mkArrow2 t1 t2 = TmPi t1 ( shift0 1 t2 )

mkArrow3 :: Term -> Term -> Term -> Term
mkArrow3 t1 t2 t3 = mkArrow2 t1 ( mkArrow2 t2 t3 )

mkArrow4 :: Term -> Term -> Term -> Term -> Term
mkArrow4 t1 t2 t3 t4 = mkArrow2 t1 ( mkArrow3 t2 t3 t4 )



tru_ = TmAbs TmStar $ TmAbs loc0 $ TmAbs loc1 loc1
fls_ = TmAbs TmStar $ TmAbs loc0 $ TmAbs loc1 loc0
_bool_ = TmPi TmStar $ mkArrow3 loc0 loc0 loc0

and_ = TmAbs _bool_ $ TmAbs _bool_ $ mkApp4 loc1 _bool_ loc0 fls_
or_ = TmAbs _bool_ $ TmAbs _bool_ $ mkApp4 loc1 _bool_ tru_ loc0
not_ = TmAbs _bool_ $ mkApp4 loc0 _bool_ fls_ tru_


_nat_ = TmPi TmStar $ mkArrow3 ( mkArrow2 loc0 loc0 ) loc0 loc0

succ_ = TmAbs _nat_ $ TmAbs TmStar $ TmAbs ( mkArrow2 loc0 loc0 ) $ TmAbs loc1 $
  mkApp2 loc1 ( mkApp4 loc3 loc2 loc1 loc0 )
plus_ = TmAbs _nat_ $ TmAbs _nat_ $ TmAbs TmStar $ TmAbs ( mkArrow2 loc0 loc0 ) $ TmAbs loc1 $
  mkApp4 loc4 loc2 loc1 ( mkApp4 loc3 loc2 loc1 loc0 )
mult_ = TmAbs _nat_ $ TmAbs _nat_ $ TmAbs TmStar $ TmAbs ( mkArrow2 loc0 loc0 ) $
  mkApp3 loc3 loc1 ( mkApp3 loc2 loc1 loc0 )
exp_ = TmAbs _nat_ $ TmAbs _nat_ $ TmAbs TmStar $ mkApp3 loc1 ( mkArrow2 loc0 loc0 ) ( mkApp2 loc2 loc0 )

c0_ = TmAbs TmStar $ TmAbs ( mkArrow2 loc0 loc0 ) $ TmAbs loc1 loc0
c1_ = mkApp2 succ_ c0_
c2_ = mkApp2 succ_ c1_
c3_ = mkApp2 succ_ c2_
c4_ = mkApp2 succ_ c3_
c5_ = mkApp2 succ_ c4_
c6_ = mkApp2 succ_ c5_
c7_ = mkApp2 succ_ c6_
c8_ = mkApp2 succ_ c7_
c9_ = mkApp2 succ_ c8_
c10_ = mkApp2 succ_ c9_
