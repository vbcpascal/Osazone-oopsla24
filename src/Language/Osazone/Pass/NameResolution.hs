{-# LANGUAGE OverloadedStrings #-}
module Language.Osazone.Pass.NameResolution where

import Language.Osazone
import Lifting.Extension
import Utils.AnsiPretty
import Utils.ErrorMessage
import Utils.Functions (filterKey, firstM, secondM)

import Control.Monad (forM)
import Control.Monad.Trans.Reader
import Data.Bitraversable (bimapM)
import Data.Functor ((<&>))
import Data.Map (Map, elems, empty, filter, filterWithKey, keys, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

nameResolution :: Lang -> Either ErrMsg Lang
nameResolution lang@Lang{..} = do
  let (proj, lib) = langModules
  let symbols = M.fromList $ map (\m -> (moduleName m, getSymbols m)) (proj ++ lib)
  let sym m = filterKey (\k -> k `elem` imports m || k == moduleName m) symbols
  let runNr m = runReaderT (nr m) (sym m, moduleName m, [])
  proj' <- mapM runNr proj
  lib' <- mapM runNr lib
  return lang { langModules = (proj', lib') }

nameResolutionForExtension :: Lang -> ExtensionFile -> Either ErrMsg ExtensionFile
nameResolutionForExtension lang@Lang{..} extfile = do
  let (proj, lib) = langModules
  let symbols = M.fromList $ map (\m -> (moduleName m, getSymbols m)) (proj ++ lib)
  let symbols' = M.insert (QName ["Lang"]) (symbols ! QName ["Lang"] ++ getSymbolInExtention extfile) symbols
  runReaderT (nr extfile) (symbols', QName ["Sugar"], [])

type Symbols = [String]
type ModuleSymbols = Map QName Symbols

getSymbols :: Module -> Symbols
getSymbols (Module {..}) = concat
  [ map (\(TypeSynonym s _ _) -> s) typeSynonym
  , concatMap (\(DataDeclaration s _ defs _) -> s : map fst defs) typeDecl
  , map semanName (elems semantics)
  , mapMaybe (snd . semanMonadType) (elems semantics)
  , map funcName (elems pureFunctions)
  , map funcName (elems monadicFunctions)
  , concatMap checkAnnos moduleAnnos
  ]
  where checkAnnos anno | withAnnotation anno "defined" = map removeParens (annoArgs anno)
        checkAnnos anno = []
        removeParens symbol
          | head symbol == '(' && last symbol == ')' = init (tail symbol)
          | otherwise = symbol

searchSymbol :: ModuleSymbols -> String -> [QName]
searchSymbol mss x = keys (M.filter (elem x) mss)

type NR a = a -> NRM a
type NREnv = (ModuleSymbols, QName, [String])
type NRM = ReaderT NREnv (Either ErrMsg)

addLocalVar :: String -> NREnv -> NREnv
addLocalVar s (ms, q, lcl) = (ms, q, s : lcl)

addLocalVars :: [String] -> NREnv -> NREnv
addLocalVars vs st = foldr addLocalVar st vs

withVar :: String -> NRM a -> NRM a
withVar s = local (addLocalVar s)

withVars :: [String] -> NRM a -> NRM a
withVars vs = local (addLocalVars vs)

class NameResolve a where
  nr :: NR a

instance NameResolve Module where
  nr mod@Module{..} = do
    typeSynonym' <- mapM nr typeSynonym
    typeDecl' <- mapM nr typeDecl
    semantics' <- mapM nr semantics
    pureFunctions' <- mapM nr pureFunctions
    monadicFunctions' <- mapM nr monadicFunctions
    return $ mod
      { typeSynonym = typeSynonym'
      , typeDecl = typeDecl'
      , semantics = semantics'
      , pureFunctions = pureFunctions'
      , monadicFunctions = monadicFunctions'
      }

instance NameResolve QName where
  nr (QName [":"]) = return (QName [":"])   -- special case: Prelude does not export (:)
  nr q@(QName [name]) = do
    (symbols, curr, lcl) <- ask
    if name `elem` lcl then return q else
      case searchSymbol symbols name of
        [] -> err' 31 $ "Undefined symbol:" <+> pretty name <|> "used in module" <+> pretty curr
        [pkg] -> return $ QName $ unwrapQName pkg ++ [name]
        pkgs -> err' 32 $ "Multiple definition of" <+> pretty name <+> "in:"
          <|> indent 2 (vsep (map pretty pkgs)) <|> "used in module" <+> pretty curr
  nr x@(QName qnames) = do
    (symbols, curr, _) <- ask
    let pkg = QName (init qnames)
        name = last qnames
    case symbols !? pkg of
      Just ss | name `elem` ss -> return x
              | otherwise -> err' 33 $ "Module" <+> pretty pkg <+> "has no symbol:" <+> viaShow name <|> "used in module" <+> pretty curr
      Nothing ->
        if head qnames == "Haskell" then return x else
          err' 34 $ "Module" <+> pretty pkg <+> "is not imported." <|> "used in module" <+> pretty curr

instance NameResolve TypeSynonym where
  nr (TypeSynonym tyName tyParams tyDef) = do
    tyDef' <- local (addLocalVars tyParams) (nr tyDef)
    return (TypeSynonym tyName tyParams tyDef')

instance NameResolve DataDeclaration where
  nr (DataDeclaration t targs tdefs annos) = do
    tdefs' <- mapM (secondM (mapM (withVars targs . nr))) tdefs
    return (DataDeclaration t targs tdefs' annos)

instance NameResolve SemanDef where
  nr (SemanDef {..}) = do
    ft' <- nr semanMatchType
    tt' <- nr semanType
    mt' <- firstM (mapM nr) semanMonadType
    def' <- forM semanDefinition \(pat, e) -> do
      pat' <- nr pat
      e' <- withVars (getLocalVars pat) (nr e)
      return (pat', e')
    return (SemanDef semanName ft' tt' mt' def')

instance NameResolve FuncDef where
  nr (FuncDef {..}) = do
    ty' <- nr funcType
    def' <- forM funcDefinition \(pats, e) -> do
      pats' <- mapM nr pats
      e' <- withVars (concatMap getLocalVars pats) (nr e)
      return (pats', e')
    return (FuncDef funcName ty' def' funcAnnotation)

instance NameResolve Type where
  nr (TyCon qname)     = TyCon <$> nr qname
  nr (TyTuple tys)     = TyTuple <$> mapM nr tys
  nr (TyList ty)       = TyList <$> nr ty
  nr (TyArrow ty1 ty2) = TyArrow <$> nr ty1 <*> nr ty2
  nr (TyApply ty1 ty2) = TyApply <$> nr ty1 <*> nr ty2
  nr t                 = return t

instance NameResolve Expr where
  nr (EVar qname) = EVar <$> nr qname
  nr (ECon qname) = ECon <$> nr qname
  nr (EVarOp qname) = EVarOp <$> nr qname
  nr (EConOp qname) = EConOp <$> nr qname
  nr (ELam pat e) = do
    pat' <- nr pat
    let vars = getLocalVars pat
    e' <- withVars vars (nr e)
    return (ELam pat' e')
  nr (EApp es) = EApp <$> mapM nr es
  nr (EMatch e brs) = EMatch <$> nr e <*> mapM nr brs
  nr (ELet bds e) = do
    (bds', vars) <- nrBindings bds
    e' <- withVars vars (nr e)
    return (ELet bds' e')
    where
      nrBindings [] = return ([], [])
      nrBindings ((pat, e) : bds) = do
        pat' <- nr pat
        let vars = getLocalVars pat
        e' <- withVars vars (nr e)
        (bds', vars') <- withVars vars (nrBindings bds)
        return ((pat', e') : bds', vars ++ vars')
  nr (EIf e1 e2 e3) = EIf <$> nr e1 <*> nr e2 <*> nr e3
  nr (ETuple es) = ETuple <$> mapM nr es
  nr (EList es) = EList <$> mapM nr es
  nr e = return e

instance NameResolve Branch where
  nr (Branch p guards e) = do
    p' <- nr p
    let vars = getLocalVars p
    guards' <- mapM (withVars vars . nr) guards
    e' <- withVars vars (nr e)
    return (Branch p' guards' e')

instance NameResolve Pattern where
  nr (PCon qname pats)   = PCon <$> nr qname <*> mapM nr pats
  nr (PConOp qname pats) = PConOp <$> nr qname <*> mapM nr pats
  nr (PTuple pats)       = PTuple <$> mapM nr pats
  nr p                   = return p

getSymbolInExtention :: ExtensionFile -> Symbols
getSymbolInExtention file = concatMap gs (extensions file)
  where gs (ExtSugars name sugars) = map sgName sugars
        gs _                       = []

instance NameResolve ExtensionFile where
  nr (ExtensionFile name exts) = ExtensionFile name <$> mapM nr exts

instance NameResolve Extension where
  nr (ExtSugars name sgs)     = ExtSugars name <$> mapM nr sgs
  nr (ExtFilters flts)        = ExtFilters <$> mapM nr flts
  nr (ExtRedefine qname defs) = return $ ExtRedefine qname defs

instance NameResolve Sugar where
  nr sugar@Sugar{..} = do
    type' <- nr sgType
    params' <- mapM nr sgParams
    let vars = concatMap getLocalVars sgParams
    def' <- (withVars vars . nr) sgDef
    return sugar { sgType = type', sgParams = params', sgDef = def' }

instance NameResolve SugarExpr where
  nr (SgExprFresh vars e) = SgExprFresh vars <$> (withVars vars . nr) e
  nr (SgExprExpr e)       = SgExprExpr <$> nr e

instance NameResolve Filter where
  nr = pure

getLocalVars :: Pattern -> [String]
getLocalVars (PVar v)            = [v]
getLocalVars (PTuple ps)         = concatMap getLocalVars ps
getLocalVars (PCon qname pats)   = concatMap getLocalVars pats
getLocalVars (PConOp qname pats) = concatMap getLocalVars pats
getLocalVars _                   = []
