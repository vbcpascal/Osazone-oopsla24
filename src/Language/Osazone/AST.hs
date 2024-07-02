{-# LANGUAGE OverloadedStrings #-}
module Language.Osazone.AST where

import Config.YamlReader (YamlConfig)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Map (Map, elems, insert, member, toList)
import Text.Read (readPrec)
import Utils.Functions (splitOn)
import Utils.Pretty

data Lang = Lang
  { langName      :: String
  , langVersion   :: String
  , langExtension :: String
  , langModules   :: ([Module], [Module])  -- Project, Lib
  , langConfig    :: YamlConfig
  } deriving (Show, Eq, Ord)

data Module = Module
  { moduleName       :: QName
  , impDecls         :: [ImportDecl]
  , typeSynonym      :: [TypeSynonym]
  , typeDecl         :: [DataDeclaration]
  , semantics        :: Map String SemanDef
  , pureFunctions    :: Map String FuncDef
  , monadicFunctions :: Map String FuncDef
  , moduleAnnos      :: [Annotation]
  } deriving (Show, Eq, Ord)

newtype QName = QName {unwrapQName :: [String]}
  deriving (Eq, Ord)

data ImportDecl
  = Import QName (Maybe String)   -- ^ like `import Eval.Meta`
  | ImportHS QName (Maybe String) -- ^ like `import Haskell.Prelude`
  deriving (Show, Eq, Ord)

data TypeSynonym
  -- | like `type Context t = Map Id t`
  = TypeSynonym String [String] Type
  deriving (Show, Eq, Ord)

data DataDeclaration = DataDeclaration
  { ddTypeName     :: String
  , ddTypeArgs     :: [String]
  , ddConstructors :: [(String, [Type])]
  , ddAnnotation   :: [Annotation]
  } deriving (Show, Eq, Ord)

data Type
  = TyCon QName
  | TyVar String
  | TyTuple [Type]
  | TyList Type
  | TyArrow Type Type
  | TyApply Type Type
  | TyForeign           -- only used in lib
  deriving (Show, Eq, Ord)

data SemanDef = SemanDef
  { semanName       :: String
  , semanMatchType  :: Type
  , semanType       :: Type
  , semanMonadType  :: ([Type], Maybe String)
  , semanDefinition :: [(Pattern, Expr)]
  } deriving (Show, Eq, Ord)

data FuncDef = FuncDef
  { funcName       :: String
  , funcType       :: Type
  , funcDefinition :: [([Pattern], Expr)]
  , funcAnnotation :: [Annotation]
  } deriving (Show, Eq, Ord)

data Expr
  = EVar QName
  | ECon QName
  | EVarOp QName
  | EConOp QName
  | ELitNum String
  | ELitStr String
  | ELam Pattern Expr
  | EApp [Expr]
  | EMatch Expr [Branch]
  | ELet [(Pattern, Expr)] Expr
  | EIf Expr Expr Expr
  | ETuple [Expr]
  | EList [Expr]
  deriving (Show, Eq, Ord)

data Branch
  = Branch Pattern [Expr] Expr
  deriving (Show, Eq, Ord)

data Pattern
  = PVar String
  | PCon QName [Pattern]
  | PConOp QName [Pattern]
  | PTuple [Pattern]
  | PList [Pattern]
  | PWildcard
  deriving (Show, Eq, Ord)

data Annotation = Annotation
  { annoTitle :: String
  , annoArgs  :: [String]
  } deriving (Show, Eq, Ord)

-- Pretty Printer

instance Show QName where
  show = intercalate "." . unwrapQName

instance Read QName where
  readsPrec _ str = [(QName (splitOn '.' str), "")]

pattern QNameN = QName []
pattern QNameI str = QName [str]
pattern QNameC head tail = QName (head : tail)

tycon :: String -> Type
tycon = TyCon . read

econ :: String -> Expr
econ = ECon . read

evar :: String -> Expr
evar = EVar . read

instance Pretty QName where
  pretty = viaShow

withPrefix :: QName -> [String] -> Bool
withPrefix _ [] = True
withPrefix (QName (x:xs)) (y:ys) | x == y = withPrefix (QName xs) ys
withPrefix _ _ = False

removePrefix :: QName -> [String] -> QName
removePrefix q [] = q
removePrefix (QName (x:xs)) (y:ys) | x == y = removePrefix (QName xs) ys
removePrefix q _ = q

instance Pretty Module where
  pretty (Module {..}) =
        "module" <+> pretty moduleName <+> "where"
    <>  line
    <|> vsep' (map pretty impDecls)
    <|> vsep' (map pretty typeSynonym)
    <|> vsep_' (map pretty typeDecl)
    <|> vsep_' (map pretty (elems semantics))
    <|> vsep_' (map pretty (elems pureFunctions))
    <|> vsep_' (map pretty (elems monadicFunctions))
    <|> vsep (map pretty moduleAnnos)

instance Pretty ImportDecl where
  pretty (Import qname alias) = "import" <+> pretty qname <+> maybe emptyDoc (\n -> "as" <+> pretty n) alias
  pretty (ImportHS qname alias) = "import" <+> pretty qname <+> maybe emptyDoc (\n -> "as" <+> pretty n) alias

instance Pretty TypeSynonym where
  pretty (TypeSynonym tname tparams ty) =
    "type" <+> pretty tname <+> hsep (map pretty tparams) <+> equals <+> pretty ty

instance Pretty DataDeclaration where
  pretty (DataDeclaration tname targs tdefs annos) =
    vsep (map pretty annos) <|>
    vsep [ "data" <+> pretty tname <+> hsep (map pretty targs)
         , indent 2 (vsep prettyDefs)
         ]
    where prettyDefs = zipWith (<+>) (equals : repeat pipe) $
            tdefs <&> \(h, ts) -> pretty h <+> hsep (map pretty ts)
          -- TODO: Do not print types of arguments now
          -- prettyParam (p, t) = parens $ pretty p <+> "::" <+> pretty t

instance Pretty SemanDef where
  pretty (SemanDef sname matchT semanT (monadT, mAlias) defs) =
    substDecl <> pretty sname <+> "for" <+> pretty matchT <+> "::" <+> pretty semanT
      <> line
      <> indent 2 (vsep (map prtMonad monadT ++ [ maybe emptyDoc (\a -> "as" <+> pretty a <> line) mAlias]))
      <|> vsep (map prtDef defs)
   where
    substDecl :: Doc ann
    substDecl | sname == "subst" = "{-# SubstitutionDeclarations #-}" <> line
              | otherwise = emptyDoc
    prtDef :: (Pattern, Expr) -> Doc ann
    prtDef (pat, expr) =
      pretty sname <+> cpPattern pat <+> equals <+> pretty expr
    prtMonad t@(TyApply {}) = "monad" <+> parens (pretty t)
    prtMonad t              = "monad" <+> pretty t

instance Pretty FuncDef where
  pretty (FuncDef fname funcT bindings annos) =
    vsep (map pretty annos)
      <|> pretty fname <+> "::" <+> pretty funcT
      <|> vsep (map prtDef bindings)
   where
    prtDef (pats, expr) = pretty fname <+> hsep (map cpPattern pats) <+> equals <+> pretty expr

instance Pretty Type where
  pretty (TyCon tname) = pretty tname
  pretty (TyVar tname) = pretty tname
  pretty (TyTuple ts) = safeTupled $ map pretty ts
  pretty (TyList t) = brackets (pretty t)
  pretty (TyArrow t1@(TyArrow {}) t2) = parens (pretty t1) <+> "->" <+> pretty t2
  pretty (TyArrow t1@(TyApply {}) t2) = parens (pretty t1) <+> "->" <+> pretty t2
  pretty (TyArrow t1 t2) = pretty t1 <+> "->" <+> pretty t2
  pretty (TyApply t1 t2@(TyArrow {})) = pretty t1 <+> parens (pretty t2)
  pretty (TyApply t1 t2@(TyApply {})) = pretty t1 <+> parens (pretty t2)
  pretty (TyApply t1 t2) = pretty t1 <+> pretty t2
  pretty TyForeign = "Foreign"

instance Pretty Expr where
  pretty (EVar vname) = pretty vname
  pretty (ECon cname) = pretty cname
  pretty (EVarOp vname) = parens $ pretty vname
  pretty (EConOp cname) = parens $ pretty cname
  pretty (ELitNum num) = pretty num
  pretty (ELitStr str) = "\"" <> pretty str <> "\""
  pretty (EApp args) = hsep (map prettyArg args)
    where prettyArg e | simpleExpr e = pretty e
                      | otherwise    = parens $ pretty e
  pretty (EMatch e@(EMatch _ _) bs) =
    "case" <+> "(" <> line <> indent 2 (pretty e) <> line <> indent 2 ") of" <|> indent 4 (vsep (map pretty bs))
  pretty (EMatch e@(ELet _ _) bs) =
    "case" <+> "(" <> indent 2 (pretty e) <> line <> indent 2 ") of" <|> indent 4 (vsep (map pretty bs))
  pretty (EMatch e bs) = "case" <+> pretty e <+> "of" <|> indent 2 (vsep (map pretty bs)) <> line
  pretty (ELet decls res) = line <> indent 2 (align $ vsep ["let" <+> align (vsep (fmap pBinding decls)), "in " <+> pretty res])
    where pBinding (x, e) = pretty x <+> equals <+> pretty e
  pretty (EIf e1 e2 e3) = "if" <+> pretty e1 <|> indent 2 ("then" <+> pretty e2) <|> indent 2 ("else" <+> pretty e3)
  pretty (ELam pat expr) = backslash <> hsep vs <+> doc
    where prettyLam x (ELam y e) = (pretty x : ds, d) where (ds, d) = prettyLam y e
          prettyLam x e          = ([pretty x, "->"], pretty e)
          (vs, doc) = prettyLam pat expr
  pretty (ETuple exprs) = safeTupled $ map pretty exprs
  pretty (EList []) = "[]"
  pretty (EList exprs) = safeList $ map pretty exprs

simpleExpr :: Expr -> Bool
simpleExpr (ECon _)   = True
simpleExpr (EVar _)   = True
simpleExpr (EConOp _) = True
simpleExpr (EVarOp _) = True
simpleExpr _          = False

instance Pretty Branch where
  pretty (Branch pat [] expr) = pretty pat <+> "->" <+> pretty expr
  pretty (Branch pat guards expr) =
    pretty pat
      <+> vsep (zipWith (<+>) (pipe : repeat comma) (map pretty guards))
      <+> "->"
      <+> pretty expr

instance Pretty Pattern where
  pretty (PVar vname)         = pretty vname
  pretty (PCon pname [])      = pretty pname
  pretty (PCon pname pargs)   = pretty pname <+> hsep (map cpPattern pargs)
  pretty (PConOp pname [])    = parens (pretty pname)
  pretty (PConOp pname pargs) = parens $ parens (pretty pname) <+> hsep (map cpPattern pargs)
  pretty (PTuple pargs)       = safeTupled $ map pretty pargs
  pretty (PList pargs)       = safeList $ map pretty pargs
  pretty PWildcard            = "_"

-- `cp` means `closed pretty`.
-- If the pattern is compound, a pair of parens will be inserted.
cpPattern :: Pattern -> Doc ann
cpPattern p@(PCon _ (_ : _)) = parens (pretty p)
cpPattern p                  = pretty p

instance Pretty Annotation where
  pretty (Annotation title args) = "[#" <> pretty title <+> hsep (map pretty args) <> "]"
    where pstr arg = "\"" <> pretty arg <> "\""
