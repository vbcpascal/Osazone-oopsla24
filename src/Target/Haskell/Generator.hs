{-# LANGUAGE OverloadedStrings #-}

module Target.Haskell.Generator where

import Language.Osazone
import Language.Osazone.Pass (readWithPass)
import Utils.AnsiPretty
import Utils.ErrorMessage
import Utils.System (createAndWriteFile)

import Control.Monad (forM, forM_)
import Control.Monad.Reader
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.List (find, intercalate, intersperse)
import Data.Map (elems)
import System.FilePath ((</>))
import Target.Haskell.InterfaceGen (generateInterfaceFiles)
import Target.Haskell.ServerGenerator (generateServer)
import Utils.Functions (mif)

generateHaskellCode :: FilePath -> IO ()
generateHaskellCode path = do
  putDocLn $ "Building language from" <+> pretty path
  lang <- readWithPass path
  let langModules = modules lang
  let totalCount = length langModules
  docs <- forM (zip langModules ([1..] :: [Int])) $ \(m, i) -> do
    putDocLn $ "[" <> pretty i <+> "of" <+> pretty totalCount <> "]"
      <+> "Building" <+> pretty (moduleName m)
    throwEitherIO $ flip runReaderT (BuilderData lang) $ gen m
  forM_ (zip langModules docs) \(mod, doc) -> do
    createAndWriteFile (targetPath path mod) (show doc)
  -- generateServer path lang
  putDocLn "Generating interface files"
  generateInterfaceFiles path
  putDocLn "Done."
 where
  targetPath path Module{..} = path </> "build/Lib" </> qnameToPath moduleName

newtype BuilderData = BuilderData
  { bdLang :: Lang }

type Builder = ReaderT BuilderData (Either ErrMsg)

class HaskellTarget a where
  gen :: a -> Builder DocAnsi

genModule :: Module -> Builder DocAnsi
genModule mod@Module{..} | isHaskellModule mod = do
    let refs = map (head . annoArgs) $ filter (`withAnnotation` "compile") moduleAnnos
    let exportModule m = "module" <+> pretty m
    let importModule m = "import" <+> pretty m
    return $ "module Lib." <> pretty moduleName
        <> line <> line
        <> indent 2 (safeTupled (map exportModule refs) <+> "where")
        <> line <> line
        <> vsep (map importModule refs)
genModule Module{..} = do
    let header = "{-# LANGUAGE LambdaCase #-}"
             <|> "{-# LANGUAGE OverloadedStrings #-}"
             <|> "module Lib." <> pretty moduleName <+> "where" <> line
    let preImports = "import Control.Monad.Trans.Class\nimport Runtime\n"
          -- <> if moduleName == read "Meta.Monad.Trans" then ""
          --    else "import Lib.Meta.Monad.Trans\n"  -- there is a special case here. Too ugly!
    impDocs   <- impDecls `decorateBy` "Imports"
    annoDocs  <- moduleAnnos `decorateBy` "Annotations"
    tsDocs    <- typeSynonym `decorateBy` "Type Synonym"
    tdDocs    <- typeDecl `decorateBy` "Data Declaration"
    semanDocs <- elems semantics `decorateBy` "Semantics"
    pFunDocs  <- decorate (genFunc False) (elems pureFunctions) "Pure Functions"
    mFunDocs  <- decorate (genFunc True) (elems monadicFunctions) "Monadic Functions"
    return $ header <|> preImports <|> impDocs <|> annoDocs <|> tsDocs <|> tdDocs
      <|> semanDocs <|> pFunDocs <|> mFunDocs

instance HaskellTarget Module where
  gen = genModule

instance HaskellTarget ImportDecl where
  gen (Import qname Nothing) =
    return $ "import qualified" <+> pretty (mapQName' qname)
  gen (ImportHS qname Nothing) =
    return $ "import qualified" <+> pretty (mapQName' qname)
  gen _ =
    raiseError (Internal NotCallPass) "Please use ImportRename and NameResolution first."

instance HaskellTarget Annotation where
  gen anno@(Annotation{..})
    | anno `withAnnotation` "defined" = return emptyDoc
    | otherwise = raiseError UnknownAnnotation $ "Unknown annotation:" <+> pretty annoTitle

instance HaskellTarget TypeSynonym where
  gen (TypeSynonym tname targs ty) = do
    tyDoc <- gen ty
    return ("type" <+> pretty tname <+> hsep (map pretty targs) <+> "=" <+> tyDoc)

instance HaskellTarget DataDeclaration where
  gen (DataDeclaration tname targs tdefs _) = do
    tdefDocs <- mapM genDefs tdefs
    return $ vsep
      [ "data" <+> pretty tname <+> hsep (map pretty targs)
      , indent 2 (vsep (zipWith (<+>) ("=" : repeat "|") tdefDocs))
      , indent 2 "deriving (Eq, Read, Show)"
      , line
      ]
   where genDefs (constr, tys) = (<+>) (pretty constr) . hsep <$> mapM gen tys

instance HaskellTarget SemanDef where
  gen SemanDef{..} = do
    let target = putMonad (fst semanMonadType ++ [tycon "Runtime"]) semanType
    mtDoc <- gen semanMatchType
    ttDoc <- gen target
    defDocs <- mapM (genSemanDef semanName) semanDefinition
    return $ vsep $ (pretty semanName <+> "::" <+> mtDoc <+> "->" <+> ttDoc) : defDocs
   where
    putMonad ts (TyArrow t1 t2) = TyArrow t1 $ putMonad ts t2
    putMonad ts t               = TyApply (foldl1 (flip TyApply) ts) t

genSemanDef :: String -> (Pattern, Expr) -> Builder DocAnsi
genSemanDef sname (pat, expr) = do
  patDoc <- gen pat
  let (lamDoc, expr') = getExpr expr
  exprDoc <- genExpr True True expr'
  return $ pretty sname <+> "_osa_pattern@" <> parens patDoc <+> "=" <+> lamDoc
    <|> indent 2 ("pushStack" <+> parens ("\"" <> pretty sname <> "\" <+>" <+> "viaShow _osa_pattern"))
    <|> indent 4 (">> popStack" <+> parens exprDoc)
  where getExpr (ELam p e) = ("\\" <> pretty p <+> "->", e)
        getExpr e          = ("", e)

genFunc :: Bool -> FuncDef -> Builder DocAnsi
genFunc mEnv FuncDef{..} | funcType /= TyForeign = do
    tDoc <- gen funcType
    let sigDoc = pretty funcName <+> "::" <+> tDoc
    defDocs <- mapM (genFuncDef mEnv funcName) funcDefinition
    return $ vsep $ sigDoc : defDocs
genFunc mEnv FuncDef{..} =
    case find ((== "gentype") . annoTitle) funcAnnotation of
      Just anno -> do
        let tstr = init $ tail $ head $ annoArgs anno  -- remove "xxx" in tstr
        let sigDoc = pretty funcName <+> "::" <+> pretty tstr
        defDocs <- mapM (genFuncDef mEnv funcName) funcDefinition
        return $ vsep $ sigDoc : defDocs
      Nothing -> raiseError MissForeignType $ "Function with ::? should have a genType annotation:" <+> pretty funcName

genFuncDef :: Bool -> String -> ([Pattern], Expr) -> Builder DocAnsi
genFuncDef mEnv sname (pats, expr) = do
  patDocs <- mapM gen pats
  exprDoc <- genExpr mEnv False expr
  return $ pretty sname <+> hsep patDocs <+> "=" <+> exprDoc

-- | In monadic generation, in semantics generation
genExpr :: Bool -> Bool -> Expr -> Builder DocAnsi
-- * Special cases
genExpr _ _ e | e == evar "Meta.Semantics.fresh" = return "fresh"

-- * EVar and EVarOp
genExpr True True e@(EVar qname) = do
  doc <- gen qname
  isMonadicExp e >>= \case
    True -> return $ parens ("lift" <+> doc)
    False -> return $ parens ("return" <+> doc)
genExpr True False e@(EVar qname) = do
  doc <- gen qname
  isMonadicExp e >>= \case
    True -> return doc
    False -> return $ parens ("return" <+> doc)
genExpr _ _ e@(EVar qname) = gen qname
genExpr True True e@(EVarOp qname) = do
  doc <- parens <$> gen qname
  isMonadicExp e >>= \case
    True -> return $ parens ("lift" <+> doc)
    False -> return $ parens ("return" <+> doc)
genExpr True False e@(EVarOp qname) = do
  doc <- gen qname
  isMonadicExp e >>= \case
    True -> return doc
    False -> return $ parens ("return" <+> doc)
genExpr _ _ e@(EVarOp qname) = parens <$> gen qname

-- * ECon and EConOp
genExpr mEnv _ (ECon qname) = gen qname >>= addReturnIfM mEnv
genExpr mEnv _ (EConOp qname) = gen qname >>= addReturnIfM mEnv . parens

-- * Literal Numbers and Strings
genExpr mEnv _ (ELitNum num) = addReturnIfM mEnv $ pretty num
genExpr mEnv _ (ELitStr str) = addReturnIfM mEnv $ "\"" <> pretty str <> "\""

-- * Application
genExpr False semanEnv (EApp args) = do
  argDocs <- mapM (genExpr False semanEnv) args
  return $ parens (hsep argDocs)
genExpr True semanEnv e@(EApp args) = do
  argDocs <- mapM (genExpr False semanEnv) args
  let doc = parens (hsep argDocs)
  isMonadicExp e >>= \case
    True  -> do
      wl <- whetherLift e
      if semanEnv && wl then return ("lift" <+> doc)
                       else return doc
    False -> return ("return" <+> doc)
  where whetherLift :: Expr -> Builder Bool
        whetherLift (EApp args) = whetherLift (head args)
        whetherLift e | e == evar "Meta.Semantics.mmap" = return False
        whetherLift (EVar qname) = do
          lang <- asks bdLang
          return (isFunction lang qname)
        whetherLift (EVarOp qname) = do
          lang <- asks bdLang
          return (isFunction lang qname)
        whetherLift _ = undefined

-- * Pattern Matching
genExpr True True (EMatch e brs) = do
  doc <- genExpr True True e
  brDocs <- mapM (genBranch True True) brs
  let merrDoc = "rr -> matchErr (viaShow rr)" <+> safeList (map genBrCond brs)
  return $ doc <+> ">>=" <+> parens ("\\case" <|> indent 2 (vsep brDocs) <|> indent 2 merrDoc)
  where genBrCond :: Branch -> DocAnsi
        genBrCond (Branch pat guards _) = "\"" <> pretty pat <> "\""
genExpr True False (EMatch e brs) = do
  doc <- genExpr True False e
  brDocs <- mapM (genBranch True False) brs
  return $ doc <+> ">>=" <+> parens ("\\case" <|> indent 2 (vsep brDocs))
genExpr False semanEnv (EMatch e brs) = do
  doc <- genExpr False semanEnv e
  brDocs <- mapM (genBranch False semanEnv) brs
  return $ "case" <+> doc <+> "of" <|> indent 2 (vsep brDocs)

-- * Let Binding
genExpr mEnv semanEnv (ELet bds e) = do
  doc <- genExpr mEnv semanEnv e
  genBindings mEnv semanEnv bds doc
 where
  genBindings _ _ [] doc = return doc
  genBindings mEnv semanEnv ((pat, e) : bindings) doc = do
    patDoc <- gen pat
    exprDoc <- genExpr mEnv semanEnv e
    doc <- genBindings mEnv semanEnv bindings doc
    genErrBranch mEnv semanEnv patDoc exprDoc doc
  genErrBranch True True patDoc exprDoc doc = do
    let merrDoc = "rr -> matchErr (viaShow rr)" <+> list ["\"" <> patDoc <> "\""]
    return $ parens exprDoc <+> ">>=" <+> parens ("\\case" <|> indent 2 (patDoc <+> "->" <+> doc) <|> indent 2 merrDoc)
  genErrBranch True False patDoc exprDoc doc =
    return $ parens exprDoc <+> ">>=" <+> parens ("\\case" <|> indent 2 (patDoc <+> "->" <+> doc))
  genErrBranch False _ patDoc exprDoc doc =
    return $ "let" <+> patDoc <+> "=" <+> exprDoc <|> indent 2 ("in" <+> doc)

-- * Tuples and Lists
genExpr mEnv semanEnv (ETuple es) =
  mapM (genExpr False semanEnv) es >>= addReturnIfM mEnv . safeTupled
genExpr mEnv semanEnv (EList es) =
  mapM (genExpr False semanEnv) es >>= addReturnIfM mEnv . safeList

-- * If-then-else
genExpr mEnv semanEnv (EIf e1 e2 e3) = do
  doc1 <- genExpr False semanEnv e1
  doc2 <- genExpr mEnv semanEnv e2
  doc3 <- genExpr mEnv semanEnv e3
  return $ parens $ "if" <+> parens doc1
    <+> ("then" <+> parens doc2)
    <+> ("else" <+> parens doc3)
  -- return $ "if" <+> parens doc1
  --   <|> indent 2 ("then" <+> parens doc2)
  --   <|> indent 2 ("else" <+> parens doc3)

genExpr _ _ _ = undefined

genPure :: Expr -> Builder DocAnsi
genPure = genExpr False False

genBranch mEnv semanEnv (Branch pat guards e) = do
  patDoc <- gen pat
  guardDocs <- mapM genPure guards
  doc <- genExpr mEnv semanEnv e
  if null guardDocs
    then return $ patDoc <+> "->" <+> doc
    else return $ patDoc <+> "|" <+> hsep (intersperse "," guardDocs) <+> "->" <+> doc


instance HaskellTarget Type where
  gen (TyCon qname) = gen qname
  gen (TyVar tname) = return (pretty tname)
  gen (TyTuple tys) = safeTupled <$> mapM gen tys
  gen (TyList t) = brackets <$> gen t
  gen (TyArrow t1 t2) = do
    t1Doc <- gen t1
    t2Doc <- gen t2
    return $ parens $ t1Doc <+> "->" <+> t2Doc
  gen (TyApply t1 t2) = do
    t1Doc <- gen t1
    t2Doc <- gen t2
    return $ parens $ t1Doc <+> t2Doc
  gen TyForeign = raiseError (Internal GenForeignType) "::? should never be used in code generation"


instance HaskellTarget Pattern where
  gen (PVar vname) = return $ pretty vname
  gen (PCon pname pargs) = do
    argDocs <- mapM gen pargs
    return $ parens $ pretty (mapQName pname) <+> hsep argDocs
  gen (PConOp pname pargs) = do
    argDocs <- mapM gen pargs
    return $ parens $ parens (pretty (mapQName pname)) <+> hsep argDocs
  gen (PTuple pats) = safeTupled <$> mapM gen pats
  gen (PList pats) = safeList <$> mapM gen pats
  gen PWildcard = return "!_"


instance HaskellTarget QName where
  gen = return . pretty . mapQName

addReturn :: DocAnsi -> DocAnsi
addReturn doc = parens ("return" <+> doc)

addReturnIf :: Bool -> DocAnsi -> DocAnsi
addReturnIf True  = addReturn
addReturnIf False = id

addReturnIfM :: Bool -> DocAnsi -> Builder DocAnsi
addReturnIfM b doc = return (addReturnIf b doc)

-- ** Helper functions in code generation

isMonadicExp :: Expr -> Builder Bool
isMonadicExp e = do
  bd <- ask
  return (isMonadic (bdLang bd) e)

-- | map QName of variables
mapQName :: QName -> QName
mapQName q@(QName [name]) = q
mapQName q                = mapQName' q

-- | map QName of modules
mapQName' :: QName -> QName
mapQName' (QName ("Haskell" : qnames)) = QName qnames
mapQName' q                            = QName $ (:) "Lib" $ unwrapQName q

-- | check Haskell modules
isHaskellModule :: Module -> Bool
isHaskellModule Module{..} = any (`withAnnotation` "compile") moduleAnnos

-- | generate doc with title comments
decorateBy :: (Traversable t, HaskellTarget a) => t a -> DocAnsi -> Builder DocAnsi
decorateBy = decorate gen

decorate :: Traversable t => (a -> Builder DocAnsi) -> t a -> DocAnsi -> Builder DocAnsi
decorate builder comps title
  | null comps = return emptyDoc
  | otherwise = do
      docs <- mapM builder (toList comps)
      return $ vsep ("--" <+> title <> line : docs) <> line
