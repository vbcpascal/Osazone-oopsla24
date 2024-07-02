{-# LANGUAGE OverloadedStrings #-}
module Language.Osazone.Parser.Translation
  ( getAST
  , getASTFromFile
  , appGetASTFromFile
  , parseOsa
  ) where

import Language.Osazone.AST as AST
import Language.Osazone.Parser.CST as CST
import Utils.ErrorMessage

import Control.Monad (when)
import Control.Monad.State
import Data.Bifunctor
import Data.Map (empty, insert, member)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Language.Osazone.Parser.ModuleParser (parseModule)
import Utils.AnsiPretty
import Utils.Pretty (Pretty (pretty))

type Translator = StateT (Module, [Annotation]) (Either ErrMsg)

translate :: File -> Translator ()
translate File{..} = do
  putName mName
  putImports imports
  translateDecls topDecls

translateDecls :: [TopDecl] -> Translator ()
translateDecls [] = pure ()
translateDecls (TopTypeSynonym ts : decls) = do
  translateDecls decls
  modify (first \m -> m { typeSynonym = ts : typeSynonym m })
translateDecls (TopDataDeclaration dd : decls) = do
  (m, annos) <- get
  put (m { typeDecl = dd { ddAnnotation = annos } : typeDecl m }, [])
  translateDecls decls
translateDecls (TopAnnotation annotation : decls) = do
  pushAnnotation annotation
  translateDecls decls
translateDecls (TypeSignature name ty : decls) = do
  (m, annos) <- get
   -- check whether f is defined
  when (name `member` pureFunctions m || name `member` monadicFunctions m) $
    err' 11 $ "Function" <+> pretty name <+> "has multiple declarations."
  -- check whether f has definitions
  let (bindings, decls') = exactBindingsWithName name decls
  when (null bindings) $ err' 12 $ "Function" <+> pretty name <+> "has no definitions."
  defs <- bindingsToDefinitions bindings
  -- check annotations
  let func = FuncDef name ty defs annos
  if withAnno "monadic" annos
    then put (m { monadicFunctions = insert name func (monadicFunctions m) }, [])
  else if withAnno "pure" annos
    then put (m { pureFunctions = insert name func (pureFunctions m) }, [])
    else err' 13 $ "Whether function" <+> pretty name <+> "is pure or monadic is unknown."
  translateDecls decls'
  where
    withAnno title = any \(Annotation title' _) -> title == title'
translateDecls (HookSignature name ft tt mt alias : decls) = do
  (m, annos) <- get
  -- check whether f is defined
  when (name `member` semantics m) $
    err' 11 $ "Semantics" <+> pretty name <+> "has multiple declarations."
  -- check whether f has definitions
  let (bindings, decls') = exactBindingsWithName name decls
  when (null bindings) $ err' 12 $ "Semantics" <+> pretty name <+> "has no definitions."
  defs <- bindingsToDefinitions bindings
  defs' <- mapM (uncurry bindingToSeman) defs
  -- update
  let seman = SemanDef name ft tt (mt, alias) defs'
  put (m { semantics = insert name seman (semantics m) }, [])
  translateDecls decls'
translateDecls (Binding name _ _ : decls) = do
  err $ "Binding " ++ name ++ " should be used after type or hook signature."

exactBindingsWithName :: String -> [TopDecl] -> ([TopDecl], [TopDecl])
exactBindingsWithName _ [] = ([], [])
exactBindingsWithName name (b@(Binding name' _ _) : decls) | name == name' =
  let (bindings, decls') = exactBindingsWithName name decls
  in  (b : bindings, decls')
exactBindingsWithName _ decls = ([], decls)

bindingsToDefinitions :: [TopDecl] -> Translator [([Pattern], Expr)]
bindingsToDefinitions decls = do
  let defs = map (\(Binding _ pats expr) -> (pats, expr)) decls
  -- TODO: need to check the patterns have the same length
  --   but there is no `allSame` function in Haskell, too bad
  return defs

putName :: QName -> Translator ()
putName qname = modify (first \m -> m { moduleName = qname })

putImports :: [ImportDecl] -> Translator ()
putImports imps = modify (first \m -> m { impDecls = imps })

pushAnnotation :: Annotation -> Translator ()
pushAnnotation anno = modify (second (anno :))

popAllAnnotation :: Translator [Annotation]
popAllAnnotation = do
  (m, annos) <- get
  put (m, [])
  return annos

forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe = flip mapMaybe

exactTypeSynonym :: File -> [TypeSynonym]
exactTypeSynonym file = mapMaybe f (topDecls file)
  where f (TopTypeSynonym ts) = Just ts
        f _                   = Nothing

exactDataDeclaration :: File -> [DataDeclaration]
exactDataDeclaration file = mapMaybe f (topDecls file)
  where f (TopDataDeclaration dd) = Just dd
        f _                       = Nothing

bindingToSeman :: [Pattern] -> Expr -> Translator (Pattern, Expr)
bindingToSeman [] _ = err "Semantics definition has no LHS."
bindingToSeman (p : ps) expr = (p,) <$> bts ps expr
  where bts :: [Pattern] -> Expr -> Translator Expr
        bts [] e = return e
        bts (PCon _ _ : _) _ = err "[Issue] Pattern matching on parameters that are not semantics targets is not allowed for now. Please use case of in RHS instead."
        bts (p : ps) e = ELam p <$> bts ps e

getAST :: File -> Either ErrMsg Module
getAST file = do
  (mod, annos) <- execStateT (translate file) (initModule, [])
  return (mod { moduleAnnos = annos })
  where initModule = Module (QName []) [] [] [] empty empty empty []

getASTFromFile :: FilePath -> String -> Either ErrMsg Module
getASTFromFile filePath src = do
  case parseModule filePath src of
    Left errMsg -> throwError OsaParseError (viaShow errMsg)
    Right file  -> getAST file

parseOsa :: FilePath -> IO Module
parseOsa path = do
  src <- readFile path
  sure (getASTFromFile path src)

appGetASTFromFile :: FilePath -> IO ()
appGetASTFromFile filePath = do
  src <- readFile filePath
  case getASTFromFile filePath src of
    Left errMsg -> putStrLn ("[test ast] error: " ++ show errMsg)
    Right mod   -> print (pretty mod)
