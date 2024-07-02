module Language.Osazone.Pass.Simplify.CaseCase where

import Language.Osazone.AST

import Data.Bifunctor (Bifunctor (second))
import Debug.Trace (trace)

flattenComp :: Expr -> Expr
flattenComp (EMatch (EMatch e brs) brs') = flattenComp (EMatch e (map f brs))
  where
    f :: Branch -> Branch
    f (Branch pat guards expr) = Branch pat guards (EMatch expr brs')
flattenComp (EMatch (ELet bindings e) brs) = flattenComp (ELet bindings (EMatch e brs))
flattenComp (EMatch e brs) =
  let e' = flattenComp e
      brs' = map (\(Branch pat guards ex) -> Branch pat guards (flattenComp ex)) brs
  in EMatch e' brs'
flattenComp (ELam x e) = ELam x (flattenComp e)
flattenComp (EApp es) = EApp $ map flattenComp es
flattenComp (ELet bds (ELet bds' e)) = flattenComp $ ELet (bds ++ bds') e
flattenComp (ELet bindings e) =
  g bindings e []
  where
    g :: [(Pattern, Expr)] -> Expr -> [(Pattern, Expr)] -> Expr
    g [] expr ok = ELet ok (flattenComp expr)
    g ((pat, ELet bds' expr') : bds) expr ok =
      flattenComp (ELet (ok ++ bds' ++ [(pat, expr')] ++ bds) expr)
    g ((pat, EMatch expr' brs) : bds) expr [] =
      flattenComp (EMatch expr' (map (putInCase pat bds expr) brs))
    g ((pat, EMatch expr' brs) : bds) expr ok =
      flattenComp (ELet ok (EMatch expr' (map (putInCase pat bds expr) brs)))
    g ((pat, e) : bds) expr ok =
      g bds expr (ok ++ [(pat, flattenComp e)])
    putInCase pat bds expr (Branch p gs ee) =
      Branch p gs (ELet ((pat, ee) : bds) expr)
flattenComp (ETuple es) = ETuple $ map flattenComp es
flattenComp (EList es) = EList $ map flattenComp es
flattenComp (EIf e1 e2 e3) = EIf (flattenComp e1) (flattenComp e2) (flattenComp e3)
flattenComp e = e

ofCourseMatchFail :: Expr -> Expr
ofCourseMatchFail (EMatch e brs) =
  EMatch e brs'
  where
    brs' = f e brs
    fGuard (EApp [EVarOp (QName ["=="]), EVar (QName ['@':v1]), EVar (QName ['@':v2])])
      | v1 /= v2 = True
    fGuard _ = False
    f _ [] = []
    f e_ (Branch _ gs _ : brs_) | any fGuard gs = f e_ brs_
    f e_ (Branch pat _ _ : brs_) | check e_ pat = f e_ brs_
    f e_ (Branch pat gs expr : brs_) = Branch pat gs (ofCourseMatchFail expr) : f e_ brs_
    check :: Expr -> Pattern -> Bool
    check (ECon q1) (PCon q2 _) | q1 /= q2 = True
    check (EApp (ECon q1 : _)) (PCon q2 _) | q1 /= q2 = True
    check (EApp (ECon _ : es)) (PCon _ ps) = any (uncurry check) (zip es ps)
    check (ECon _) (PTuple {}) = True
    check (ETuple _) (PCon {}) = True
    check _ _ = False
ofCourseMatchFail (ELam x e) = ELam x (ofCourseMatchFail e)
ofCourseMatchFail (EApp es) = EApp $ map ofCourseMatchFail es
ofCourseMatchFail (ELet bindings e) =
  let bindings' = map (second ofCourseMatchFail) bindings
      e' = ofCourseMatchFail e
  in ELet bindings' e'
ofCourseMatchFail (ETuple es) = ETuple $ map ofCourseMatchFail es
ofCourseMatchFail (EList es) = EList $ map ofCourseMatchFail es
ofCourseMatchFail (EIf e1 e2 e3) = EIf (ofCourseMatchFail e1) (ofCourseMatchFail e2) (ofCourseMatchFail e3)
ofCourseMatchFail e = e

-- Wildcard matching cannot be simplified because some changes
-- may happen
checkMatch :: Expr -> Pattern -> Bool
checkMatch (ETuple []) PWildcard = True
checkMatch (ECon qname1) (PCon qname2 [])
  | qname1 == qname2 = True
checkMatch (EApp (ECon qname1 : es)) (PCon qname2 ps)
  | qname1 == qname2 = all (uncurry checkMatch) (zip es ps)
checkMatch _ _ = False

ofCourseMatch :: Expr -> Expr
ofCourseMatch (EMatch e (Branch pat [] expr : _)) | checkMatch e pat =
  ofCourseMatch expr
ofCourseMatch (EMatch e brs) =
  let e' = ofCourseMatch e
      brs' = map (\(Branch pat guards ex) -> Branch pat guards (ofCourseMatch ex)) brs
  in EMatch e' brs'
ofCourseMatch (ELam x e) = ELam x (ofCourseMatch e)
ofCourseMatch (EApp es) = EApp $ map ofCourseMatch es
ofCourseMatch (ELet bindings e) = case bindings' of
  [] -> e'
  _  -> ELet bindings' e'
  where
    -- f (pat, expr) bds | checkMatch expr pat = bds
    -- f bd bds = bd : bds
    -- bindings' = foldr f [] bindings
    bindings' = concatMap (uncurry genBindings) bindings
    e' = ofCourseMatch e

    genBindings :: Pattern -> Expr -> [(Pattern, Expr)]
    genBindings PWildcard (ETuple []) = []
    genBindings (PTuple ps) (ETuple es) = concatMap (uncurry genBindings) (zip ps es)
    genBindings (PCon qname2 []) (ECon qname1) | qname1 == qname2 = []
    genBindings (PCon qname2 ps) (EApp (ECon qname1 : es)) | qname1 == qname2 =
      concatMap (uncurry genBindings) (zip ps (map ofCourseMatch es))
    genBindings p e = [(p, ofCourseMatch e)]

ofCourseMatch (ETuple es) = ETuple $ map ofCourseMatch es
ofCourseMatch (EList es) = EList $ map ofCourseMatch es
ofCourseMatch (EIf e1 e2 e3) | alwaysTrue e1 = ofCourseMatch e2
  where alwaysTrue (ECon (QName ["Prelude", "True"])) = True
        alwaysTrue (EApp [e1, e2, e3]) | e1 == EVarOp (QName ["Prelude", "=="]), e2 == e3 = True
        alwaysTrue _ = False
ofCourseMatch (EIf e1 e2 e3) = EIf (ofCourseMatch e1) (ofCourseMatch e2) (ofCourseMatch e3)
ofCourseMatch e = e
