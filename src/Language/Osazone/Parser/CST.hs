module Language.Osazone.Parser.CST
  ( module Language.Osazone.Parser.CST
  , module Language.Osazone.AST
  ) where

import Language.Osazone.AST

data File = File
  { mName    :: QName
  , imports  :: [ImportDecl]
  , topDecls :: [TopDecl]
  } deriving (Show)

data TopDecl
  = TopTypeSynonym TypeSynonym
  | TopDataDeclaration DataDeclaration
  | TopAnnotation Annotation
  | TypeSignature String Type
  | HookSignature String Type Type [Type] (Maybe String)
  | Binding String [Pattern] Expr
  deriving (Show)
