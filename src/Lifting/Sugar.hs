{-# LANGUAGE OverloadedStrings #-}
module Lifting.Sugar where

import Language.Osazone
import Utils.Pretty

import Data.List (intersperse)

data Sugar = Sugar
  { sgName       :: String
  , sgType       :: Type
  , sgParamTypes :: [Type]
  , sgParams     :: [Pattern]
  , sgDef        :: SugarExpr
  } deriving (Eq, Ord, Show)

data SugarExpr
  = SgExprFresh [String] SugarExpr
  | SgExprExpr Expr
  deriving (Eq, Ord, Show)

instance Pretty Sugar where
  pretty Sugar{..} =
    pretty sgName <+> hsep (map pretty sgParamTypes) <+> "::" <+> pretty sgType <|>
    pretty sgName <+> hsep (map (parens . pretty) sgParams) <+> "=" <+> pretty sgDef

instance Pretty SugarExpr where
  pretty (SgExprFresh xs e) = "@fresh" <+> hsep (map pretty xs) <+> "in" <|> indent 2 (pretty e)
  pretty (SgExprExpr e) = pretty e
