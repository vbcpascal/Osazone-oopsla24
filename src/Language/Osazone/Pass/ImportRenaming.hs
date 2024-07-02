module Language.Osazone.Pass.ImportRenaming where

import Control.Monad.Reader
import Data.Bitraversable (bimapM)
import Data.Map (Map, fromList, member, (!))
import Data.Maybe (mapMaybe)
import Language.Osazone.AST
import Utils.Functions

importRenaming' :: Lang -> Lang
importRenaming' lang@(Lang {..}) =
  let (proj, lib) = langModules
      proj' = map importRenaming proj
      lib' = map importRenaming lib
  in lang { langModules = (proj', lib') }

importRenaming :: Module -> Module
importRenaming mod@(Module {..}) =
  let dict = fromList $ mapMaybe getRename impDecls
  in  prelude (runReader (ir mod) dict)
  where getRename :: ImportDecl -> Maybe (String, QName)
        getRename (Import qname (Just alias))   = Just (alias, qname)
        getRename (ImportHS qname (Just alias)) = Just (alias, qname)
        getRename _                             = Nothing

prelude :: Module -> Module
prelude mod = mod { impDecls =
    Import (QName ["Prelude"]) Nothing
  : impDecls mod }

type IR a = a -> Reader (Map String QName) a

class ImportRename a where
  ir :: IR a
  ir = pure

instance ImportRename QName where
  ir q@(QName [x, y]) = do
    dict <- ask
    if x `member` dict
    then return $ QName $ unwrapQName (dict ! x) ++ [y]
    else return q
  ir q = return q

instance ImportRename Module where
  ir mod@(Module {..}) = do
    let imp' = map removeAlias impDecls
    ts' <- mapM ir typeSynonym
    dd' <- mapM ir typeDecl
    semans' <- mapMapM ir semantics
    pfuncs' <- mapMapM ir pureFunctions
    mfuncs' <- mapMapM ir monadicFunctions
    return (Module moduleName imp' ts' dd' semans' pfuncs' mfuncs' moduleAnnos)
    where removeAlias (Import qname _)   = Import qname Nothing
          removeAlias (ImportHS qname _) = ImportHS qname Nothing

instance ImportRename TypeSynonym where
  ir (TypeSynonym t targs ty) = do
    ty' <- ir ty
    return (TypeSynonym t targs ty')

instance ImportRename DataDeclaration where
  ir dd@DataDeclaration{..} = do
    tdefs' <- mapM (secondM (mapM ir)) ddConstructors
    return dd { ddConstructors = tdefs' }

instance ImportRename SemanDef where
  ir (SemanDef {..}) = do
    ft' <- ir semanMatchType
    tt' <- ir semanType
    mt' <- firstM (mapM ir) semanMonadType
    def' <- mapM (bimapM ir ir) semanDefinition
    return (SemanDef semanName ft' tt' mt' def')

instance ImportRename FuncDef where
  ir (FuncDef {..}) = do
    ty' <- ir funcType
    def' <- mapM (bimapM (mapM ir) ir) funcDefinition
    return (FuncDef funcName ty' def' funcAnnotation)

instance ImportRename Type where
  ir (TyCon qname)     = TyCon <$> ir qname
  ir (TyTuple tys)     = TyTuple <$> mapM ir tys
  ir (TyList ty)       = TyList <$> ir ty
  ir (TyArrow ty1 ty2) = TyArrow <$> ir ty1 <*> ir ty2
  ir (TyApply ty1 ty2) = TyApply <$> ir ty1 <*> ir ty2
  ir t                 = return t

instance ImportRename Expr where
  ir (EVar qname)   = EVar <$> ir qname
  ir (ECon qname)   = ECon <$> ir qname
  ir (EVarOp qname) = EVarOp <$> ir qname
  ir (EConOp qname) = EConOp <$> ir qname
  ir (ELam pat e)   = ELam <$> ir pat <*> ir e
  ir (EApp es)      = EApp <$> mapM ir es
  ir (EMatch e brs) = EMatch <$> ir e <*> mapM ir brs
  ir (ELet bds e)   = ELet <$> mapM (bimapM ir ir) bds <*> ir e
  ir (EIf e1 e2 e3) = EIf <$> ir e1 <*> ir e2 <*> ir e3
  ir (ETuple es)    = ETuple <$> mapM ir es
  ir (EList es)     = EList <$> mapM ir es
  ir e              = return e

instance ImportRename Branch where
  ir (Branch p guards e) = Branch <$> ir p <*> mapM ir guards <*> ir e

instance ImportRename Pattern where
  ir (PCon qname pats) = PCon <$> ir qname <*> mapM ir pats
  ir (PTuple pats)     = PTuple <$> mapM ir pats
  ir p                 = return p
