{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Lifting.Parser where

import Language.Osazone
import Language.Osazone.Parser.Combinators
import Language.Osazone.Parser.GeneralParser
    ( pAtomType
    , pBinding
    , pClosePattern
    , pExpr
    , pImportDecl
    , parseFile
    )
import Language.Osazone.Parser.Scanner (Token (..))
import Lifting.Extension
import Lifting.Filter
import Lifting.Sugar

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Functor (($>))
import Text.Parsec
import Utils.AnsiPretty (Pretty (pretty), putDocLn)

pExtensionFile :: P ExtensionFile
pExtensionFile = do
  exact KwModule
  qname <- QName <$> conId
  exact KwWhere
  exts <- braces (pExtension `sepBy1` semicolon)
  return $ ExtensionFile qname exts
  -- ExtensionFile <$> many pExtension

pExtension :: P Extension
pExtension = notQualified varId >>= \case
  "sugar" -> do
    name <- notQualified varId
    exact KwWhere
    ExtSugars name <$> braces (pSugar `sepBy1` semicolon)
  "filter" -> do
    exact KwWhere
    ExtFilters <$> braces (pFilter `sepBy1` semicolon)
  "redefine" -> do
    qname <- QName <$> varId
    exact KwWhere
    ExtRedefine qname <$> braces pRedefine
  _ -> undefined

-- | The parser for a syntactic sugar definition.
-- | (Note: this parser is not complete, it requires that the signature of a syntactic
-- | sugar must be followed by the definition.)
pSugar :: P Sugar
pSugar = do
  name <- notQualified conId
  tys <- many pAtomType
  exact KwOpDoubleColon
  ty <- pAtomType
  semicolon
  name' <- notQualified conId
  pats <- many pClosePattern
  exact KwOpEquals
  e <- pSugarExpr
  return (Sugar name ty tys pats e)

pSugarExpr :: P SugarExpr
pSugarExpr = do
    exact KwFresh
    vars <- many (notQualified varId)
    exact KwIn
    e <- pSugarExpr
    return (SgExprFresh vars e)
  <|> SgExprExpr <$> pExpr

pFilter :: P Filter
pFilter = do
  mode <- notQualified varId
  str <- notQualified varId
  build mode str <$>
    (try (parens (exact KwOpDoubleDot) $> [])
      <|> parens (notQualified conId `sepBy1` exact Comma))
  where
    build "use" str []  = FilterUseAll str
    build "use" str qs  = FilterUse str qs
    build "hide" str [] = FilterHideAll str
    build "hide" str qs = FilterHide str qs
    build _ _ _         = undefined

pRedefine :: P [([Pattern], Expr)]
pRedefine = pDef `sepBy1` semicolon
  where
    pDef = do
      id <- notQualified varId   -- TODO: check this id
      pats <- many pClosePattern
      exact KwOpEquals
      expr <- pExpr
      return (pats, expr)

parseExtensionFile :: FilePath -> String -> Either ParseError ExtensionFile
parseExtensionFile = parseFile pExtensionFile

parseExtensionFile' :: String -> Either ParseError ExtensionFile
parseExtensionFile' = parseFile pExtensionFile "anonymous.sgs"

appParseExtensionFile :: FilePath -> IO ()
appParseExtensionFile filepath = do
  src <- readFile filepath
  case parseExtensionFile filepath src of
    Left errMsg -> putStrLn ("[parse ext] error: " ++ show errMsg)
    Right file  -> putDocLn $ pretty file
