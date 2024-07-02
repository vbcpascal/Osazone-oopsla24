{-# HLINT ignore "Use <$>" #-}
module Language.Osazone.Parser.ModuleParser where

import Language.Osazone.Parser.Combinators
import Language.Osazone.Parser.CST
import Language.Osazone.Parser.GeneralParser
import Language.Osazone.Parser.Token

import Text.Parsec

pTopDecl :: P TopDecl
pTopDecl = TopTypeSynonym <$> pTypeSynonym
  <|> TopDataDeclaration <$> pDataDecl
  <|> TopAnnotation <$> pAnnotation
  <|> try do -- type signature
    id <- notQualified varId
    exact KwOpDoubleColon
    ty <- pType
    return (TypeSignature id ty)
  <|> try do -- foreign type signature
    id <- notQualified varId
    exact KwOpDoubleColonQues
    return (TypeSignature id TyForeign)
  <|> try do -- hook signature
    id <- notQualified varId
    exact KwFor
    ft <- pType
    exact KwOpDoubleColon
    tt <- pType
    mt <- many (exact KwMonad *> pType)
    alias <- optionMaybe (exact KwAs *> notQualified conId)
    return (HookSignature id ft tt mt alias)
  <|> header "binding" do
    id <- notQualified varId
    pats <- many pClosePattern
    exact KwOpEquals
    expr <- pExpr
    return (Binding id pats expr)

pFile :: P File
pFile = do
  exact KwModule
  qname <- QName <$> conId
  exact KwWhere
  (imports, decls) <- try ((,) [] <$> braceList pTopDecl)
    <|> braces do
          imports <- pImportDecl `sepBy1'` semicolon
          semicolon
          decls <- pTopDecl `sepBy1` semicolon
          return (imports, decls)
  return (File qname imports decls)

parseModule :: FilePath -> String -> Either ParseError File
parseModule = parseFile pFile

parseModule' :: String -> Either ParseError File
parseModule' = parseFile pFile "anonymous.osa"

appParseModule :: FilePath -> IO ()
appParseModule filePath = do
  src <- readFile filePath
  case parseModule filePath src of
    Left errMsg -> putStrLn ("[parse cst] error: " ++ show errMsg)
    Right file  -> print file
