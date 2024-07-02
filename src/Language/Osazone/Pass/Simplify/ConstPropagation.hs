module Language.Osazone.Pass.Simplify.ConstPropagation where

import Language.Osazone.AST

import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor (Bifunctor (second))
import Data.Char (isNumber)
import Data.Map (Map, empty, insert, (!?))

constPropagation :: Expr -> Expr
constPropagation e = transGuards $ runReader (cpExpr e) empty

type VarMap = Map QName Expr
type R = Reader VarMap

cpExpr :: Expr -> Reader VarMap Expr
cpExpr (EVar qname) = do
  env <- ask
  case env !? qname of
    Nothing -> return (EVar qname)
    Just e  -> return e
cpExpr (EMatch e [Branch (PVar qname) guards expr]) | isConst e = do
  let fe = local (insert (QName [qname]) e)
  guards' <- mapM (fe . cpExpr) guards
  expr' <- fe (cpExpr expr)
  cpExpr $ EMatch (ETuple []) [Branch PWildcard guards' expr']
cpExpr (EMatch e brs) = do
  e' <- cpExpr e
  brs' <- forM brs $ \(Branch p gs expr) -> do
    gs' <- mapM cpExpr gs
    expr' <- cpExpr expr
    return (Branch p gs' expr')
  return (EMatch e' brs')
cpExpr (ELet bindings expr) = do
  f bindings []
  where
    f :: [(Pattern, Expr)] -> [(Pattern, Expr)] -> R Expr
    f [] [] = cpExpr expr
    f [] ok = ELet ok <$> cpExpr expr
    -- Actually all non-isMonadic e can be eliminated
    f ((PWildcard, e) : bds) ok | isConst e =
      f bds ok
    f ((PVar qname, e) : bds) ok | isConst e =
      local (insert (QName [qname]) e) (f bds ok)
    f ((pat, e) : bds) ok = do
      e' <- cpExpr e
      f bds (ok ++ [(pat, e')])
cpExpr (EApp es) = EApp <$> mapM cpExpr es
cpExpr (ETuple es) = ETuple <$> mapM cpExpr es
cpExpr (EList es) = EList <$> mapM cpExpr es
cpExpr (ELam x e) = ELam x <$> cpExpr e
cpExpr (EIf e1 e2 e3) = EIf <$> cpExpr e1 <*> cpExpr e2 <*> cpExpr e3
cpExpr e = return e

isConst :: Expr -> Bool
isConst (ECon _)    = True
isConst (EConOp _)  = True
isConst (EApp args) = all isConst args
isConst (EList es)  = all isConst es
isConst _           = False

transGuards :: Expr -> Expr
transGuards (EMatch (ETuple [])
  [Branch _ [EApp [EVarOp (QName ["=="]), e1, e2]] e]) | e1 == e2 = transGuards e
transGuards (EMatch e brs) =
  EMatch (transGuards e) (map (\(Branch pat gs expr) -> Branch pat gs (transGuards expr)) brs)
transGuards (ELet bindings expr) =
  ELet (map (second transGuards) bindings) (transGuards expr)
transGuards (ELam x expr) = ELam x (transGuards expr)
transGuards e = e
