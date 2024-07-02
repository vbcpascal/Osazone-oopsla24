module Language.Osazone.Pass.Simplify.AliasPropagation where

import Language.Osazone

import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor (Bifunctor (second))
import Data.Map (Map, empty, insert, (!?))

aliasPropagation :: Lang -> Expr -> Expr
aliasPropagation lang e = runReader (apExpr e) (lang, empty)

type VarMap = Map QName Expr
type R = Reader (Lang, VarMap)

apExpr :: Expr -> R Expr
apExpr (EVar qname) = do
  (_, env) <- ask
  case env !? qname of
    Nothing         -> return (EVar qname)
    Just e@(EVar _) -> apExpr e
    Just e          -> return e
apExpr (EMatch e brs) = do
  e' <- apExpr e
  brs' <- forM brs $ \(Branch p gs expr) -> do
    gs' <- mapM apExpr gs
    expr' <- apExpr expr
    return (Branch p gs' expr')
  return (EMatch e' brs')
apExpr (ELet bindings expr) = do
  (lang, _) <- ask
  f lang bindings []
  where
    f :: Lang -> [(Pattern, Expr)] -> [(Pattern, Expr)] -> R Expr
    f lang [] [] = apExpr expr
    f lang [] ok = ELet ok <$> apExpr expr
    f lang ((PVar v, e@(EVar (QName [s]))) : bds) ok =
      local (second (insert (QName [v]) e)) (f lang bds ok)
    f lang ((pat, e) : bds) ok = do
      e' <- apExpr e
      f lang bds (ok ++ [(pat, e')])
apExpr (EApp es) = EApp <$> mapM apExpr es
apExpr (ETuple es) = ETuple <$> mapM apExpr es
apExpr (EList es) = EList <$> mapM apExpr es
apExpr (ELam x e) = ELam x <$> apExpr e
apExpr (EIf e1 e2 e3) = EIf <$> apExpr e1 <*> apExpr e2 <*> apExpr e3
apExpr e = return e
