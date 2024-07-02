{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Language.Osazone.Parser.GeneralParser where

import Language.Osazone.Parser.Combinators
import Language.Osazone.Parser.CST
import Language.Osazone.Parser.Layout (Tok (Concrete), process)
import Language.Osazone.Parser.Scanner (scan)
import Language.Osazone.Parser.Token

import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Data.List (intercalate)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Parsec hiding (satisfy)

-----------------------------------------------------------
-- Top Declarations
-----------------------------------------------------------

pImportDecl :: P ImportDecl
pImportDecl = do
  exact KwImport
  strs <- conId
  rename <- optionMaybe (exact KwAs *> notQualified conId)
  if head strs == "Haskell"
    then return (ImportHS (QName strs) rename)
    else return (Import (QName strs) rename)
  <?> "import declaration"

pTypeSynonym :: P TypeSynonym
pTypeSynonym = do
  exact KwType
  id <- notQualified conId
  params <- many (notQualified varId)
  exact KwOpEquals
  ty <- pType
  return (TypeSynonym id params ty)
  <?> "type synonym"

pDataDecl :: P DataDeclaration
pDataDecl = do
  exact KwData
  id <- notQualified conId
  params <- many (notQualified varId)
  defs <- option [] (do
    exact KwOpEquals
    ((,) <$> notQualified conId <*> many pAtomType) `sepBy1` exact KwOpPipe)
  return (DataDeclaration id params defs [])
  <?> "data declaration"

pAnnotation :: P Annotation
pAnnotation = do
  exact OpenAnnotation
  title <- notQualified varId
  args <- catMaybes <$> many annoArg
  optional semicolon
  exact CloseBracket
  return (Annotation title args)
  where
    annoArg = try do
          toks <- parens (many annoToken)
          let content = concat $ mapMaybe prContent toks
          return (Just ("(" ++ content ++ ")"))
      <|> prContent <$> annoToken
    annoToken = satisfy \case
      Concrete _ tok | tok `elem` [OpenParenthesis, CloseParenthesis, OpenBracket, CloseBracket] -> False
      Concrete _ _ -> True
      _ -> False
    prContent (Concrete _ token) = Just (show token)
    prContent _                  = Nothing

-----------------------------------------------------------
-- Types & Patterns & Expressions & Statements
-----------------------------------------------------------

pType :: P Type
pType = try (TyArrow <$> pAppType <* exact KwOpArrow <*> pType)
  <|> pAppType
  <?> "type"

pAppType :: P Type
pAppType = handleApp <$> many pAtomType
  where handleApp []  = TyTuple []
        handleApp [t] = t
        handleApp ts  = foldl1 TyApply ts

pAtomType :: P Type
pAtomType = TyCon . QName <$> conId
  <|> TyVar <$> notQualified varId
  <|> TyList <$> brackets pType
  <|> handleParens <$> parens (try pType `sepBy1` exact Comma)
  where handleParens [x] = x
        handleParens xs  = TyTuple xs

-- Close pattern requires that a compound pattern must be parened
pClosePattern :: P Pattern
pClosePattern = PCon . QName <$> conId <*> pure []
  <|> try (parens (PCon . QName <$> conId <*> many atomPattern))
  <|> try (PConOp . QName <$> parens conOp <*> pure [])
  <|> try (parens (PConOp . QName <$> parens conOp <*> many atomPattern))
  <|> atomPattern

pPattern :: P Pattern
pPattern = PCon . QName <$> conId <*> many atomPattern
  <|> try (PConOp . QName <$> parens conOp <*> many atomPattern)
  <|> atomPattern
  <?> "pattern"

atomPattern :: P Pattern
atomPattern = PVar <$> notQualified varId
  <|> PWildcard <$ exact KwWildcard
  <|> try (PList [] <$ exact OpenBracket <* exact CloseBracket)
  <|> PList <$> brackets (pPattern `sepBy` exact Comma)
  <|> handleParens <$> parens (try pPattern `sepBy1` exact Comma)
  where handleParens [p] = p
        handleParens ps  = PTuple ps

pBinding :: P (Pattern, Expr)
pBinding = (,) <$> pPattern <* exact KwOpEquals <*> pExpr
  <?> "binding"

pExpr :: P Expr
pExpr = try do
    e1 <- pExpr'
    op <- pOp
    e2 <- pExpr'
    return (EApp [op, e1, e2])
  <|> pExpr'

pOp :: P Expr
pOp = EVarOp . QName <$> varOp
  <|> EConOp . QName <$> conOp

pExpr' :: P Expr
pExpr' = header "pattern matching" do
    exact KwCase
    e <- pExpr
    exact KwOf
    brs <- braceList pBranch
    return (EMatch e brs)
  <|> header "let binding" do
    exact KwLet
    bindings <- braceList pBinding
    exact KwIn
    e <- pExpr
    return (ELet bindings e)
  <|> header "if-then-else" do
    exact KwIf
    e1 <- pExpr
    exact KwThen
    e2 <- pExpr
    exact KwElse
    e3 <- pExpr
    return (EIf e1 e2 e3)
  <|> header "lambda expression" do
    exact KwOpLambda
    pats <- many pClosePattern
    exact KwOpArrow
    e <- pExpr
    return (foldr ELam e pats)
  <|> header "application" (handleApp <$> many atomExpr)
  <?> "expressions"
  where handleApp []  = ETuple []
        handleApp [x] = x
        handleApp xs  = EApp xs

atomExpr :: P Expr
atomExpr = EVar . QName <$> varId
  <|> ECon . QName <$> conId
  <|> try (EVarOp . QName <$> parens varOp)
  <|> try (EConOp . QName <$> parens conOp)
  <|> ELitNum <$> litNum
  <|> ELitStr <$> litStr
  <|> handleParens <$> parens (try pExpr `sepBy1` exact Comma)
  <|> try (EList [] <$ exact OpenBracket <* exact CloseBracket)
  <|> EList <$> brackets (pExpr `sepBy` exact Comma)
  where handleParens [e] = e
        handleParens es  = ETuple es

pBranch :: P Branch
pBranch = header "branch" do
  pat <- pPattern
  guards <- option [] (exact KwOpPipe *> pExpr `sepBy1` exact Comma)
  exact KwOpArrow
  e <- pExpr
  return (Branch pat guards e)

parseFile :: P a -> FilePath -> String -> Either ParseError a
parseFile parser filePath = scan filePath >=> pure . process >=> parse parser filePath
