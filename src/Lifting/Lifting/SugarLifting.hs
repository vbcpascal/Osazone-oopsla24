module Lifting.Lifting.SugarLifting where

import Language.Osazone
import Language.Osazone.Pass.Rename (runRename)
import Lifting.Sugar (Sugar (..), SugarExpr (..))
import Utils.Functions (assert, mif)
import Utils.Pretty (prettyShow)

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM)
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Functor ((<&>))
import Data.List (singleton)
import Data.Map (Map, empty, insert, keys, member, unions, (!))
import Data.Maybe (fromJust, isJust)
import Debug.Trace

newtype LiftingSugarEnv = LiftingSugarEnv { core :: Lang }
type LiftingSugar = Reader LiftingSugarEnv

liftSugar :: Sugar -> Lang -> Lang
liftSugar sugar@Sugar{..} =
  work (addLanguageConstruct sugar) >.> work (deriveSemantics sugar)
  where work f lang = runReader f (LiftingSugarEnv lang)
        (>.>) = flip (.)

addLanguageConstruct :: Sugar -> LiftingSugar Lang
addLanguageConstruct sugar = do
  lang@Lang{..} <- asks core
  let projMods = fst langModules <&> \m ->
        if isLangModule m then addConstructorToLang sugar m else m
  return lang { langModules = (projMods, snd langModules) }

deriveSemantics :: Sugar -> LiftingSugar Lang
deriveSemantics sugar = do
  lang@Lang{..} <- asks core
  projMods <- forM (fst langModules) (deriveSemanticsInModule sugar)
  return lang { langModules = (projMods, snd langModules) }

-- * Functions to add new language construct

isLangModule :: Module -> Bool
isLangModule Module{..} = moduleName == QName ["Lang"]

addConstructorToLang :: Sugar -> Module -> Module
addConstructorToLang sugar mod@Module{..} =
  mod { typeDecl = addConstructorToDecls sugar typeDecl }
  where
    addConstructorToDecls Sugar{..} [] = error $ "Type " ++ show sgType ++ " not found in language."
    addConstructorToDecls sugar@Sugar{..} decls = decls <&> \decl ->
      if isTargetType sugar decl then addConstructorToTarget sugar decl else decl
    addConstructorToTarget Sugar{..} dd@DataDeclaration {..} = -- (DataDeclaration tname targs tdefs annos) =
      if sgName `elem` map fst ddConstructors then dd
      else dd { ddConstructors = ddConstructors ++ [(sgName, sgParamTypes)] }
    isTargetType Sugar{..} (DataDeclaration tname _ _ _) = sgType == TyCon (QName ["Lang", tname])
    matchQName qname qname' = nameOfIdent qname == nameOfIdent qname'

-- * Function to derive semantics

deriveSemanticsInModule :: Sugar -> Module -> LiftingSugar Module
deriveSemanticsInModule sugar@Sugar{..} mod@Module{..} = do
  semans <- mapM (deriveSemanticsDef sugar moduleName) semantics
  return mod { semantics = semans }

deriveSemanticsDef :: Sugar -> QName -> SemanDef -> LiftingSugar SemanDef
deriveSemanticsDef sugar@Sugar{..} (QName qname) seman@SemanDef{..}
  | sgType `asMatchtype` semanMatchType = do
    let lhsPattern = PCon (QName ["Lang", sgName]) sgParams
    expr <- deriveSemanticsExpr sgParams [] (EVar (QName (qname ++ [semanName]))) sgDef
    return seman { semanDefinition = semanDefinition ++ [(lhsPattern, expr)]}
deriveSemanticsDef _ _ seman = return seman

deriveSemanticsExpr :: [Pattern] -> [String] -> Expr -> SugarExpr -> LiftingSugar Expr
deriveSemanticsExpr pats used f (SgExprFresh vars sgExpr) = do
  expr <- deriveSemanticsExpr pats (used ++ vars) f sgExpr
  return $ ELet (map (\x -> (PVar x, EVar (QName ["Meta", "Semantics", "fresh"]))) vars) expr
deriveSemanticsExpr pats used f (SgExprExpr expr) = do
  lang <- asks core
  return $ flip evalState (RewriteEnv lang used) $ do
    mapM_ putVarInPatternsToUsed pats
    rewrite (EApp [f, expr])

fakeNameResolution :: Expr -> Expr
fakeNameResolution (ECon (QName [qname])) = ECon (QName ["Lang", qname])
fakeNameResolution e@ECon{}               = e
fakeNameResolution (EApp es)              = EApp $ map fakeNameResolution es
fakeNameResolution e                      = e

asMatchtype :: Type -> Type -> Bool
asMatchtype (TyCon qname) (TyCon qname')
  | nameOfIdent qname == nameOfIdent qname' = True
asMatchtype _ _ = False


data RewriteEnv = RewriteEnv
  { langDef  :: Lang
  , usedVars :: [String]
  } deriving Show

type Rewrite = State RewriteEnv

rewrite :: Expr -> Rewrite Expr
rewrite e@EVar{} = pure e
rewrite e@ECon{} = pure e
rewrite e@EVarOp{} = pure e
rewrite e@EConOp{} = pure e
rewrite e@ELitNum{} = pure e
rewrite e@ELitStr{} = pure e
rewrite e@(ELam pat expr) = ELam pat <$> rewrite expr
rewrite e@(EMatch expr branches) =
  EMatch <$> rewrite expr <*> mapM rewriteBranch branches
  where rewriteBranch (Branch pat guards expr) =
          Branch pat <$> mapM rewrite guards <*> rewrite expr
rewrite e@(ELet bindings expr) =
  ELet <$> mapM rewriteBinding bindings <*> rewrite expr
  where rewriteBinding (pat, expr) = (pat,) <$> rewrite expr
rewrite e@(EIf e1 e2 e3) =
  EIf <$> rewrite e1 <*> rewrite e2 <*> rewrite e3
rewrite (ETuple es) = ETuple <$> mapM rewrite es
rewrite (EList es) = EList <$> mapM rewrite es
rewrite (EApp es) = do
  e <- EApp <$> mapM rewrite es
  -- TODO: simplify e here
  mif (expandable e) (expand e) (return e)

expandable :: Expr -> Rewrite Bool
expandable (EApp (EVar (QName ["Meta", "Semantics", "mmap"]) : es)) = do
  return (isList (es !! 1))
expandable (EApp (e : es)) = do
  c1 <- isSemanticsExpression e
  c2 <- isExpandableFun e
  c <- isLanguageConstruct (head es)
  -- let !_ = trace (show [c1,c2,c] ++ ";  " ++ show (e:es)) ()
  let res = (c1 || c2) && (c || isList (head es))
  return res
expandable e = isExpandableFun e

isList :: Expr -> Bool
isList (EList _) = True
isList _         = False

expand :: Expr -> Rewrite Expr
expand (EApp [EVar (QName ["Meta", "Semantics", "mmap"]), e, EList []]) =
  return (EList [])
expand (EApp [EVar (QName ["Meta", "Semantics", "mmap"]), e, EList es]) = do
  let bindings = zipWith (\n e' -> (PVar ("inner" ++ show n), EApp [e, e'])) [1..] es
  let res = ELet bindings $ EList $ zipWith (\n e -> EVar (QName ["inner" ++ show n])) [1..] es
  rewrite res
expand (EApp (e : es)) = do
  def <- getDefinition e
  assert "The length of patterns in definition and that of arguments should be matched."
    (length es == length (fst (head def)))
  es' <- mapM renameArg es
  applyDefinition def es' >>= \case
    Just e' -> do
      vs <- gets usedVars
      rewrite e'
    Nothing -> return (EApp (e : es))
  where renameArg expr = do
          env@RewriteEnv{..} <- get
          let (expr', vars') = runRename usedVars expr
          put (env {usedVars = vars'})
          return expr'
expand e = expand (EApp [e])

applyDefinition :: SomeDefinition -> [Expr] -> Rewrite (Maybe Expr)
applyDefinition defs args = do
  -- let !_ = trace (show (map fst defs) ++ ";  " ++ show args) ()
  res <- matchBranch defs args
  case res of
    Just (def, subst) -> do
      env@RewriteEnv{..} <- get
      let (def', vars') = runRename (usedVars ++ keys subst) def
      put (env {usedVars = vars'})
      return (Just (applySubstitution subst def'))
    Nothing -> return Nothing

matchBranch :: SomeDefinition -> [Expr] -> Rewrite (Maybe (Expr, Map String Expr))
matchBranch [] e =
  -- let !_ = trace ("No branch matched: " ++ show e) () in
  return Nothing
matchBranch ((pats, def) : brs) exprs =
  -- let res = trace (show (pats, exprs)) (zipWith matchPattern pats exprs) in
  -- let !_ = trace (show res) () in
  let res = zipWith matchPattern pats exprs in
  if all isJust res then return (Just (def, unions (map fromJust res))) else matchBranch brs exprs

applySubstitution :: Map String Expr -> Expr -> Expr
applySubstitution subst e@(EVar (QName [x]))
  | x `member` subst = subst ! x
  | otherwise = e
applySubstitution subst (EApp es) = EApp (map (applySubstitution subst) es)
applySubstitution subst (EMatch e branches) =
  EMatch (applySubstitution subst e) (map (applySubstBranch subst) branches)
  where applySubstBranch subst (Branch pats guards expr) =
          Branch pats (map (applySubstitution subst) guards) (applySubstitution subst expr)
applySubstitution subst (ELet bindings e) =
  ELet (map (applySubstBinding subst) bindings) (applySubstitution subst e)
  where applySubstBinding subst (pat, expr) = (pat, applySubstitution subst expr)
applySubstitution subst (EIf e1 e2 e3) =
  EIf (applySubstitution subst e1) (applySubstitution subst e2) (applySubstitution subst e3)
applySubstitution subst (ELam x e) = ELam x (applySubstitution subst e)
applySubstitution subst (ETuple es) = ETuple (map (applySubstitution subst) es)
applySubstitution subst (EList es) = EList (map (applySubstitution subst) es)
applySubstitution subst e = e

-- `isSemanticsExpression Eval.eval = True`
isSemanticsExpression :: Expr -> Rewrite Bool
isSemanticsExpression (EVar qname) = do
  lang <- gets langDef
  return (isSemantics lang qname)
isSemanticsExpression _ = return False

-- `isExpandableFun Subst.subst = True`
isExpandableFun :: Expr -> Rewrite Bool
isExpandableFun (EVar qname) = do
  lang <- gets langDef
  return $ (&&) (isFunction lang qname)
    $ any (`withAnnotation` "expand")
    $ funcAnnotation $ funOfIdent lang qname
isExpandableFun _ = return False

-- `isLanguageConstruct (Lang.EIf ...) = true`
isLanguageConstruct :: Expr -> Rewrite Bool
isLanguageConstruct (ECon _)            = return True
isLanguageConstruct (EApp (ECon _ : _)) = return True
isLanguageConstruct _                   = return False

type SomeDefinition = [([Pattern], Expr)]

-- get the definition (list of patterns and expressions) of EVar
getDefinition :: Expr -> Rewrite SomeDefinition
getDefinition e@(EVar qname) = do
  lang <- gets langDef
  return $ fromJust $ funcDefinition <$> funOfIdentMaybe lang qname
    <|> toSomeDefinition . semanDefinition <$> semanticsOfIdentMaybe lang qname
    <|> error ("Definition not found: " ++ show qname)
  where toSomeDefinition = map (uncurry tds)
        tds pat (ELam pat' e) = ([pat, pat'], e)
        tds pat e             = ([pat], e)
getDefinition _ = undefined

-- put variable into used variables set
putUsed :: String -> Rewrite ()
putUsed x = gets (\env -> env {usedVars = x : usedVars env}) >>= put

-- similar to above, but for patterns
putVarInPatternsToUsed :: Pattern -> Rewrite ()
putVarInPatternsToUsed (PVar x)        = putUsed x
putVarInPatternsToUsed (PCon _ pats)   = mapM_ putVarInPatternsToUsed pats
putVarInPatternsToUsed (PConOp _ pats) = mapM_ putVarInPatternsToUsed pats
putVarInPatternsToUsed (PTuple pats)   = mapM_ putVarInPatternsToUsed pats
putVarInPatternsToUsed PWildcard       = return ()
